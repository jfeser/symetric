open Ast
open Search_state
open Foldm

module U =
  Unshare.Make
    (G)
    (struct
      let kind = Node.match_ ~args:(fun _ -> `Args) ~state:(fun _ -> `State)
    end)

module G = U.G_replicated
module C = Cone.Make (U.G_replicated)

module V_foldable = struct
  type t = G.t

  type elem = G.V.t

  let fold g ~init ~f = G.fold_vertex (fun x acc -> f acc x) g init
end

module E_foldable = struct
  type t = G.t

  type elem = G.E.t

  let fold g ~init ~f = G.fold_edges_e (fun x acc -> f acc x) g init
end

module Refinement = struct
  type t = { old : Args.t; new_ : Set.M(Abs).t } [@@deriving sexp_of]

  let pp fmt x = Sexp.pp_hum fmt @@ [%sexp_of: t] x
end

let normalize_bits bits =
  Map.of_alist_multi (module Int) bits
  |> Map.mapi ~f:(fun ~key ~data ->
         List.reduce_exn data ~f:(fun b b' ->
             match (b, b') with
             | Some x, Some y ->
                 if Bool.(x = y) then Some x
                 else
                   Error.create "Bit value conflict" (key, b, b')
                     [%sexp_of: int * bool option * bool option]
                   |> Error.raise
             | None, (Some _ as b) | (Some _ as b), None -> b
             | None, None -> None))
  |> Map.to_alist

module Vars = struct
  type t = {
    edge_vars : String_id.t Map.M(G.E).t;
    var_edges : G.E.t Map.M(String_id).t;
    state_vars : Symb.t Map.M(State).t;
    arg_vars : Symb.t Map.M(Args).t;
  }

  let make_bit_vars prefix =
    List.init (Lazy.force Global.n_bits) ~f:(fun b ->
        Smt.fresh_decl ~prefix:(prefix b) ())
    |> Smt.all

  let make graph vertex_rel =
    let open Smt.Let_syntax in
    let%bind edge_vars =
      let module F = FoldM0 (Smt) (E_foldable) in
      F.fold graph
        ~init:(Map.empty (module G.E))
        ~f:(fun m e ->
          let%map decl = Smt.fresh_decl ~prefix:"e" () in
          Map.set m ~key:e ~data:decl)
    in

    let var_edges =
      Map.to_alist edge_vars |> List.map ~f:Tuple.T2.swap
      |> Map.of_alist_exn (module String_id)
    in

    let%bind state_vars, arg_vars =
      let module F = FoldM0 (Smt) (V_foldable) in
      F.fold graph
        ~init:(Map.empty (module State), Map.empty (module Args))
        ~f:(fun (ms, ma) v ->
          let node = vertex_rel.Unshare.One_to_many.backward v in
          let type_ = Node.type_ node in
          Node.match_ node
            ~args:(fun args_v ->
              let%map vars =
                Symb.create ~prefix:(sprintf "a%d_b%d_" (Args.id args_v)) type_
              in
              (ms, Map.set ma ~key:args_v ~data:vars))
            ~state:(fun state_v ->
              let%map vars =
                Symb.create
                  ~prefix:(sprintf "s%d_b%d_" (State.id state_v))
                  type_
              in
              (Map.set ms ~key:state_v ~data:vars, ma)))
    in

    return { edge_vars; var_edges; state_vars; arg_vars }
end

open Vars

let check_true interpolant v state =
  let open Smt.Let_syntax in
  let prob =
    let%bind () = Smt.clear_asserts in
    let%bind () = Smt.(assert_ (not (interpolant => var v))) in
    Smt.check_sat
  in
  not (Smt.eval_with_state state prob)

let check_false interpolant v state =
  let open Smt.Let_syntax in
  let prob =
    let%bind () = Smt.clear_asserts in
    let%bind () = Smt.(assert_ (not (interpolant => not (var v)))) in
    Smt.check_sat
  in
  not (Smt.eval_with_state state prob)

let forced_bits interpolant state =
  let vars = Smt.Expr.vars interpolant |> Set.to_list in
  if !Global.enable_forced_bit_check then
    List.map vars ~f:(fun var ->
        if check_true interpolant var state then (var, Some true)
        else if check_false interpolant var state then (var, Some false)
        else (var, None))
  else List.map vars ~f:(fun v -> (v, None))

let refinement_of_interpolant graph separator interpolant vars =
  let open Refinement in
  let models = Smt.Expr.models interpolant in

  let open Option.Let_syntax in
  let refinement =
    Set.to_list separator
    |> List.filter_map ~f:(fun args_v ->
           let var = Map.find_exn vars.arg_vars args_v in
           let old_states =
             Node.of_args args_v |> G.pred graph
             |> List.map ~f:(fun v -> State.state @@ Node.to_state_exn v)
             |> Set.of_list (module Abs)
           in

           let new_state_sets =
             Set.to_list old_states
             |> List.map ~f:(fun old_state -> Symb.refine models old_state var)
           in

           print_s
             [%message
               "refining"
                 (args_v : Args.t)
                 (old_states : Set.M(Abs).t)
                 (new_state_sets : Set.M(Abs).t list)];

           if List.exists new_state_sets ~f:Set.is_empty then None
           else
             let new_ = Set.union_list (module Abs) new_state_sets in
             if Set.is_subset new_ ~of_:old_states then None
             else Some Refinement.{ old = args_v; new_ })
  in

  Fmt.epr "Refinement: %a@." Fmt.Dump.(list Refinement.pp) refinement;
  if List.is_empty refinement then None else Some refinement

let assert_group_var g v = Smt.(Interpolant.assert_group ~group:g (var v))

let vars_to_exprs = List.map ~f:Smt.var

let find_edge_vars vars = List.map ~f:(Map.find_exn vars.edge_vars)

let pred_selected vars g v =
  G.pred_e g v |> find_edge_vars vars |> vars_to_exprs

let succ_selected vars g v =
  G.succ_e g v |> find_edge_vars vars |> vars_to_exprs

let filter g ~f = G.fold_vertex (fun v vs -> if f v then v :: vs else vs) g []

let to_list g = G.fold_vertex (fun v vs -> v :: vs) g []

let assert_input_states_contained group vars =
  let open Smt in
  Map.to_alist vars.state_vars
  |> List.map ~f:(fun (state_v, var) ->
         let state = State.state state_v in
         Symb.contained var ~by:state
         |> fresh_defn ~prefix:(Fmt.str "state-%d" @@ State.id state_v)
         >>= assert_group_var group)
  |> all_unit

let assert_output_state_contained group vars target_node expected =
  let open Smt in
  let var = Map.find_exn vars.state_vars target_node in
  fresh_defn ~prefix:"correct-output"
    (Symb.contained var ~by:(Abs.lift expected))
  >>= assert_group_var group

let assert_selected_state_selects_input group vars graph state_v =
  let open Smt in
  let v = Node.of_state state_v in
  let parent_select =
    if G.in_degree graph v > 0 then or_ @@ pred_selected vars graph v else true_
  and child_select =
    if G.out_degree graph v > 0 then exactly_one @@ succ_selected vars graph v
    else true_
  in
  let body = parent_select => child_select
  and name = sprintf "state-%d-deps" (State.id state_v) in
  fresh_defn ~prefix:name body >>= assert_group_var group

let assert_selected_args_selects_inputs group vars graph args_v =
  let open Smt in
  let v = Node.of_args args_v in
  let parent_select =
    if G.in_degree graph v > 0 then or_ @@ pred_selected vars graph v else true_
  and child_select = succ_selected vars graph v in
  let body = parent_select => and_ child_select
  and name = sprintf "args-%d-deps" (Args.id args_v) in
  fresh_defn ~prefix:name body >>= assert_group_var group

let assert_state_semantics group vars graph v =
  let open Smt in
  let body =
    let state_vars = Map.find_exn vars.state_vars v in
    G.succ_e graph (Node.of_state v)
    |> List.map ~f:(fun ((_, _, v) as e) ->
           let args_v = Node.to_args_exn v in
           let args_vars = Map.find_exn vars.arg_vars args_v in
           let is_selected = Map.find_exn vars.edge_vars e |> var in
           is_selected => Symb.(args_vars = state_vars))
    |> and_
  and name = sprintf "state-%d-semantics" (State.id v) in
  fresh_defn ~prefix:name body >>= assert_group_var group

let assert_args_semantics group vars graph v =
  let open Smt in
  let open Let_syntax in
  let op = Args.op v in
  let incoming_edges = G.succ_e graph (Node.of_args v) in

  let incoming_states =
    List.map incoming_edges ~f:(fun (_, n, v) -> (Node.to_state_exn v, n))
    |> List.sort ~compare:(fun (_, n) (_, n') -> [%compare: int] n n')
    |> List.map ~f:(fun (v, _) -> Map.find_exn vars.state_vars v)
  in

  let out = Map.find_exn vars.arg_vars v in
  let name = Fmt.str "semantics-%a-%d" Op.pp op (Args.id v) in

  if Op.arity op <> List.length incoming_states then (
    Dump.dump_detailed ~suffix:"error" graph;
    raise_s
      [%message "unexpected args" (op : Op.t) (incoming_states : Symb.t list)] );

  let%bind eval_result = Symb.eval op incoming_states in
  fresh_defn ~prefix:name Symb.(eval_result = out) >>= assert_group_var group

let in_separator s v =
  Node.match_ ~args:(Fun.const false)
    ~state:(List.mem s ~equal:[%compare.equal: State.t])
    v

module FV = FoldM0 (Smt) (V_foldable)
module F = FoldM2 (Smt) (Set)

let synth_constrs graph rel target_node expected_output
    (separator : Set.M(Args).t) =
  let module G = U.G_replicated in
  let open Smt.Let_syntax in
  (* Create interpolant groups for the lower and upper parts of the graph *)
  let%bind lo_group = Smt.Interpolant.Group.create
  and hi_group = Smt.Interpolant.Group.create in

  let top_graph =
    let g = G.copy graph in

    (* Remove separator dependency edges. *)
    Set.iter separator ~f:(fun args_v ->
        Node.of_args args_v |> R.forward
        |> Sequence.iter ~f:(G.iter_succ_e (G.remove_edge_e g) g));

    let target = Node.of_state target_node |> R.forward |> Sequence.hd_exn in
    C.cone g target (Set.to_list separator)
  in
  (* Dump.dump_detailed ~suffix:"top-graph"
   *   (\* ~separator:(List.mem separator ~equal:[%equal: Node.t]) *\)
   *   top_graph; *)
  let%bind vars = Vars.make top_graph in

  let%bind () =
    let%bind () =
      assert_output_state_contained hi_group vars target_node expected_output
    in
    let%bind () = assert_input_states_contained hi_group vars in
    FV.iter top_graph ~f:(fun v ->
        Node.match_ v
          ~state:(fun v ->
            let%bind () =
              assert_selected_state_selects_input hi_group vars top_graph v
            in
            assert_state_semantics hi_group vars top_graph v)
          ~args:(fun v ->
            if Set.mem separator v then return ()
            else
              let%bind () =
                assert_selected_args_selects_inputs hi_group vars top_graph v
              in
              assert_args_semantics hi_group vars top_graph v))
  in

  let%bind () =
    F.iter separator ~f:(fun args_v ->
        (* Dump.dump_detailed ~suffix:"full-graph"
         *   (\* ~separator:(List.mem separator ~equal:[%equal: Node.t]) *\)
         *   graph; *)
        let local_graph = cone graph (Node.of_args args_v) [] in

        (* Dump.dump_detailed ~suffix:"local-graph"
         *   (\* ~separator:(List.mem separator ~equal:[%equal: Node.t]) *\)
         *   local_graph; *)
        let%bind local_vars =
          let%map vs = Vars.make local_graph in
          {
            vs with
            arg_vars =
              Map.set vs.arg_vars ~key:args_v
                ~data:(Map.find_exn vars.arg_vars args_v);
          }
        in

        let%bind () = assert_input_states_contained lo_group vars in

        FV.iter local_graph ~f:(fun v ->
            Node.match_ v
              ~state:(fun v ->
                let%bind () =
                  assert_selected_state_selects_input lo_group local_vars
                    local_graph v
                in
                assert_state_semantics lo_group local_vars local_graph v)
              ~args:(fun v ->
                let%bind () =
                  assert_selected_args_selects_inputs lo_group local_vars
                    local_graph v
                in
                assert_args_semantics lo_group local_vars local_graph v)))
  in

  let%bind vars_of_group = Smt.Interpolant.group_vars in
  let ivars = Set.inter (vars_of_group lo_group) (vars_of_group hi_group) in
  print_s [%message "interpolant vars" (ivars : Set.M(Smt.Var).t)];

  return (vars, lo_group)

let process_interpolant graph separator vars interpolant =
  let interpolant = ok_exn interpolant in
  Fmt.epr "Interpolant: %a@." Smt.Expr.pp interpolant;
  match refinement_of_interpolant graph separator interpolant vars with
  | Some r -> Some r
  | None -> None

let process_model vars model =
  List.filter_map model ~f:(fun (e, is_selected) ->
      if is_selected then Map.find vars.var_edges e else None)
  |> Set.of_list (module G.E)

let get_refinement state target_node expected_output separator =
  (* Select the subset of the graph that can reach the target *)
  let graph =
    let module C = Cone (G) in
    C.cone state (Node.of_state target_node) []
  in

  (* Filter the separator to this graph subset *)
  let separator =
    Set.filter separator ~f:(fun v -> G.mem_vertex graph @@ Node.of_args v)
  in

  (* Remove sharing in the graph subset *)
  let graph, rel = U.unshare graph in

  Dump.dump_detailed ~suffix:"before-refine"
    ~separator:(Node.match_ ~state:(fun _ -> false) ~args:(Set.mem separator))
    graph;

  let (vars, solver_output), _ =
    let open Smt in
    let open Let_syntax in
    let get_interpolant =
      let%bind vars, sep_group =
        synth_constrs graph target_node expected_output separator
      in
      let%bind ret = Smt.get_interpolant_or_model [ sep_group ] in
      return (vars, ret)
    in
    Smt.run get_interpolant
  in

  Either.map solver_output
    ~first:(process_interpolant graph separator vars)
    ~second:(process_model vars)
