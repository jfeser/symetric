open Ast
open Params
open Search_state
open Foldm

module U =
  Unshare.Make
    (G)
    (struct
      let kind = Node.match_ ~args:(fun _ -> `Args) ~state:(fun _ -> `State)
    end)

open Unshare.One_to_many
module G = U.G_replicated
module UFold = Graph_ext.Folds (U.G_replicated)
module UCone = Cone.Make (U.G_replicated)
module USeparator = Separator.Make (U.G_replicated)

let dump_detailed ~suffix ?separator ss graph rel =
  let (module Attr) = attr ss in
  let open Dump.Make
             (U.G_replicated)
             (struct
               let vertex_name v = Fmt.str "%d" @@ U.id v

               let vertex_attributes v =
                 Attr.vertex_attributes @@ rel.backward v
             end) in
  dump_detailed ~suffix ?separator (params ss) graph

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
  type t = Set.M(Abs).t Map.M(Args).t [@@deriving sexp_of]

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
    state_vars : Symb.t Map.M(G.V).t;
    arg_vars : Symb.t Map.M(G.V).t;
  }

  let make_bit_vars params prefix =
    List.init params.n_bits ~f:(fun b -> Smt.fresh_decl ~prefix:(prefix b) ())
    |> Smt.all

  let make ss graph vertex_rel =
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
        ~init:(Map.empty (module G.V), Map.empty (module G.V))
        ~f:(fun (ms, ma) v ->
          let node = vertex_rel.Unshare.One_to_many.backward v in
          let type_ = Node.type_ ss node in
          Node.match_ node
            ~args:(fun args_v ->
              let%map vars =
                Symb.create (params ss)
                  ~prefix:(sprintf "a%d_b%d_" (Args.id args_v))
                  type_
              in
              (ms, Map.set ma ~key:v ~data:vars))
            ~state:(fun state_v ->
              let%map vars =
                Symb.create (params ss)
                  ~prefix:(sprintf "s%d_b%d_" (State.id state_v))
                  type_
              in
              (Map.set ms ~key:v ~data:vars, ma)))
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

let forced_bits params interpolant state =
  let vars = Smt.Expr.vars interpolant |> Set.to_list in
  if params.enable_forced_bit_check then
    List.map vars ~f:(fun var ->
        if check_true interpolant var state then (var, Some true)
        else if check_false interpolant var state then (var, Some false)
        else (var, None))
  else List.map vars ~f:(fun v -> (v, None))

let inter_partition p p' =
  Set.to_sequence p
  |> Sequence.concat_map ~f:(fun v ->
         Set.to_sequence p'
         |> Sequence.map ~f:(Abs.meet v)
         |> Sequence.filter ~f:(Fun.negate Abs.is_bottom))
  |> Sequence.to_list
  |> Set.of_list (module Abs)

let refinement_of_interpolant ss graph rel separator interpolant lower_constr
    vars =
  let open Refinement in
  let open Option.Let_syntax in
  let refinement =
    Set.to_list separator
    |> List.map ~f:(fun v ->
           let var = Map.find_exn vars.arg_vars v in
           let old_states =
             G.pred graph v
             |> List.map ~f:(fun v ->
                    State.state ss @@ Node.to_state_exn @@ rel.backward v)
             |> Set.of_list (module Abs)
           in

           let new_ =
             Set.to_list old_states
             |> List.map ~f:(fun old_state ->
                    Symb.refine (params ss) interpolant lower_constr old_state
                      var)
             |> Set.union_list (module Abs)
           in
           let old = Node.to_args_exn @@ rel.backward v in
           (old, new_))
    |> Map.of_alist_reduce (module Args) ~f:inter_partition
  in

  print_s [%message (refinement : Refinement.t)];

  refinement

let assert_group_var g v = Smt.(Interpolant.assert_group ~group:g (var v))

let vars_to_exprs = List.map ~f:Smt.var

let find_edge_vars vars = List.map ~f:(Map.find_exn vars.edge_vars)

let pred_selected vars g v =
  G.pred_e g v |> find_edge_vars vars |> vars_to_exprs

let succ_selected vars g v =
  G.succ_e g v |> find_edge_vars vars |> vars_to_exprs

let filter g ~f = G.fold_vertex (fun v vs -> if f v then v :: vs else vs) g []

let to_list g = G.fold_vertex (fun v vs -> v :: vs) g []

let assert_input_states_contained ss group vars rel =
  let open Smt in
  Map.to_alist vars.state_vars
  |> List.map ~f:(fun (v, var) ->
         let state_v = Node.to_state_exn @@ rel.backward v in
         let state = State.state ss state_v and id = State.id state_v in
         Symb.contained (params ss) var ~by:state
         |> fresh_defn ~prefix:(Fmt.str "state-%d" id)
         >>= assert_group_var group)
  |> all_unit

let assert_output_state_contained params group vars target_node expected =
  let open Smt in
  let var = Map.find_exn vars.state_vars target_node in
  fresh_defn ~prefix:"correct-output"
    (Symb.contained params var ~by:(Abs.lift expected))
  >>= assert_group_var group

let assert_selected_state_selects_input group vars graph rel v =
  let open Smt in
  let id =
    let state_v = Node.to_state_exn @@ rel.backward v in
    State.id state_v
  in
  let parent_select =
    if G.in_degree graph v > 0 then or_ @@ pred_selected vars graph v else true_
  and child_select =
    if G.out_degree graph v > 0 then exactly_one @@ succ_selected vars graph v
    else true_
  in
  let body = parent_select => child_select
  and name = sprintf "state-%d-deps" id in
  fresh_defn ~prefix:name body >>= assert_group_var group

let assert_selected_args_selects_inputs group vars graph rel v =
  let open Smt in
  let id =
    let args_v = Node.to_args_exn @@ rel.backward v in
    Args.id args_v
  in
  let parent_select =
    if G.in_degree graph v > 0 then or_ @@ pred_selected vars graph v else true_
  and child_select = succ_selected vars graph v in
  let body = parent_select => and_ child_select
  and name = sprintf "args-%d-deps" id in
  fresh_defn ~prefix:name body >>= assert_group_var group

let assert_state_semantics group vars graph rel v =
  let open Smt in
  let id =
    let state_v = Node.to_state_exn @@ rel.backward v in
    State.id state_v
  in
  let state_vars = Map.find_exn vars.state_vars v in
  let body =
    G.succ_e graph v
    |> List.map ~f:(fun ((_, _, v) as e) ->
           let args_vars = Map.find_exn vars.arg_vars v in
           let is_selected = Map.find_exn vars.edge_vars e |> var in
           is_selected => Symb.(args_vars = state_vars))
    |> and_
  and name = sprintf "state-%d-semantics" id in
  fresh_defn ~prefix:name body >>= assert_group_var group

let assert_args_semantics ss group vars graph rel v =
  let open Smt in
  let open Let_syntax in
  let op, id =
    let args_v = Node.to_args_exn @@ rel.backward v in
    (Args.op ss args_v, Args.id args_v)
  in

  let incoming_states =
    G.succ_e graph v
    |> List.map ~f:(fun (_, n, v) -> (v, n))
    |> List.sort ~compare:(fun (_, n) (_, n') -> [%compare: int] n n')
    |> List.map ~f:(fun (v, _) -> Map.find_exn vars.state_vars v)
  in

  let out = Map.find_exn vars.arg_vars v in
  let name = Fmt.str "semantics-%a-%d" Op.pp op id in

  if Op.arity op <> List.length incoming_states then
    raise_s
      [%message "unexpected args" (op : _ Op.t) (incoming_states : Symb.t list)];

  let%bind eval_result = Symb.eval (params ss) op incoming_states in
  fresh_defn ~prefix:name Symb.(eval_result = out) >>= assert_group_var group

let in_separator s v =
  Node.match_ ~args:(Fun.const false)
    ~state:(List.mem s ~equal:[%compare.equal: State.t])
    v

module FV = FoldM0 (Smt) (V_foldable)
module F = FoldM2 (Smt) (Set)

let synth_constrs ss graph rel target_node expected_output separator =
  let module G = U.G_replicated in
  let open Smt.Let_syntax in
  (* Create interpolant groups for the lower and upper parts of the graph *)
  let%bind lo_group = Smt.Interpolant.Group.create
  and hi_group = Smt.Interpolant.Group.create in

  (* There should only be one target even after unsharing *)
  let target_node =
    Node.of_state target_node |> rel.forward |> Sequence.hd_exn
  in

  let top_graph =
    let g = G.copy graph in

    (* Remove separator dependency edges. *)
    Set.iter separator ~f:(G.iter_succ_e (G.remove_edge_e g) g);

    UCone.cone g [ target_node ]
  in

  let separator = Set.filter separator ~f:(G.mem_vertex top_graph) in

  [%test_pred: Set.M(G.V).t] ~message:"separator not in top graph"
    (Set.for_all ~f:(G.mem_vertex top_graph))
    separator;

  let m_vars = Vars.make ss top_graph rel in
  let%bind vars = m_vars in

  let%bind () =
    let%bind () =
      assert_output_state_contained (params ss) hi_group vars target_node
        expected_output
    in
    (* let%bind () = assert_input_states_contained ss hi_group vars rel in *)
    FV.iter top_graph ~f:(fun v ->
        Node.match_ (rel.backward v)
          ~state:(fun _ ->
            let%bind () =
              assert_selected_state_selects_input hi_group vars top_graph rel v
            in
            assert_state_semantics hi_group vars top_graph rel v)
          ~args:(fun _ ->
            if Set.mem separator v then return ()
            else
              let%bind () =
                assert_selected_args_selects_inputs hi_group vars top_graph rel
                  v
              in
              assert_args_semantics ss hi_group vars top_graph rel v))
  in

  let lower_constr =
    F.iter separator ~f:(fun v ->
        let local_graph = UCone.cone graph [ v ] in

        let%bind local_vars =
          let%map vs = Vars.make ss local_graph rel in
          {
            vs with
            arg_vars =
              Map.set vs.arg_vars ~key:v ~data:(Map.find_exn vars.arg_vars v);
          }
        in

        let%bind () = assert_input_states_contained ss lo_group vars rel in
        FV.iter local_graph ~f:(fun v ->
            Node.match_ (rel.backward v)
              ~state:(fun _ ->
                let%bind () =
                  assert_selected_state_selects_input lo_group local_vars
                    local_graph rel v
                in
                assert_state_semantics lo_group local_vars local_graph rel v)
              ~args:(fun _ ->
                let%bind () =
                  assert_selected_args_selects_inputs lo_group local_vars
                    local_graph rel v
                in
                assert_args_semantics ss lo_group local_vars local_graph rel v)))
  in
  let%bind () = lower_constr in
  return
    ( vars,
      lo_group,
      let%bind _ = m_vars in
      lower_constr )

let process_interpolant params graph rel separator vars interpolant lower_constr
    =
  let interpolant = ok_exn interpolant in
  Fmt.epr "Interpolant: %a@." Smt.Expr.pp interpolant;
  refinement_of_interpolant params graph rel separator interpolant lower_constr
    vars

let rec extract_program ss graph rel selected_edges target =
  let args =
    G.succ_e graph target
    |> List.filter ~f:(Set.mem selected_edges)
    |> List.map ~f:(fun (_, _, v) -> v)
  in
  match args with
  | [ a ] ->
      `Apply
        ( Args.op ss @@ Node.to_args_exn @@ rel.backward a,
          G.succ graph a
          |> List.map ~f:(extract_program ss graph rel selected_edges) )
  | args ->
      let args =
        List.map args ~f:(fun v -> Node.to_args_exn @@ rel.backward v)
      in
      raise_s [%message "expected one argument" (args : Args.t list)]

let process_model vars model ss graph rel target =
  let selected_edges =
    List.filter_map model ~f:(fun (e, is_selected) ->
        if is_selected then Map.find vars.var_edges e else None)
    |> Set.of_list (module G.E)
  in
  extract_program ss graph rel selected_edges target

let run_solver params graph rel target_node expected_output separator =
  let open Smt in
  let open Let_syntax in
  let get_interpolant =
    let%bind vars, sep_group, lower_constr =
      synth_constrs params graph rel target_node expected_output separator
    in
    let%bind ret = Smt.get_interpolant_or_model [ sep_group ] in
    return (vars, ret, lower_constr)
  in
  let ret, _ = Smt.run get_interpolant in
  ret

let get_refinement ss target_node =
  (* Select the subset of the graph that can reach the target *)
  let graph =
    let module C = Cone.Make (Search_state.G) in
    C.cone (graph ss) [ Node.of_state target_node ]
  in

  Search_state.dump_detailed_graph ~suffix:"before-unsharing" ss graph;

  (* Remove sharing in the graph subset *)
  let graph, rel = U.unshare graph in
  let top =
    Option.value_exn ~message:"graph does not have a greatest element"
      (UFold.V.find graph ~f:(fun v -> G.in_degree graph v = 0))
  in

  dump_detailed ss ~suffix:"after-unsharing" graph rel;

  let expected_output = Conc.bool_vector (params ss).bench.output in

  let m_refinement =
    USeparator.simple graph top
    |> Sequence.find_map ~f:(fun separator ->
           match
             run_solver ss graph rel target_node expected_output separator
           with
           | vars, First interpolant, lower_constr ->
               process_interpolant ss graph rel separator vars interpolant
                 lower_constr
               |> Option.some
           | _ -> None)
  in

  match m_refinement with
  | Some r -> First r
  | None -> (
      match
        run_solver ss graph rel target_node expected_output
          (Set.empty (module G.V))
      with
      | _, First _, _ -> failwith "expected model"
      | vars, Second model, _ ->
          Second (process_model vars model ss graph rel top) )
