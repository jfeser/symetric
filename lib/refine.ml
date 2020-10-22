open Ast
open Search_state

module FoldM0
    (M : Monad.S) (F : sig
      type t

      type elem

      val fold_left : t -> init:'b -> f:('b -> elem -> 'b) -> 'b
    end) =
struct
  let fold ~f ~init c =
    let open M.Let_syntax in
    F.fold_left
      ~f:(fun acc x ->
        let%bind acc = acc in
        f acc x)
      ~init:(return init) c

  let iter ~f c = fold ~f:(fun () x -> f x) ~init:() c
end

module FoldM
    (M : Monad.S) (F : sig
      type 'a t

      val fold_left : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
    end) =
struct
  let fold (type a) ~f ~init (c : a F.t) =
    let module F0 = struct
      type t = a F.t

      type elem = a

      let fold_left = F.fold_left
    end in
    let module Fold = FoldM0 (M) (F0) in
    Fold.fold ~f ~init c
end

module V_foldable = struct
  type t = G.t

  type elem = G.V.t

  let fold_left g ~init ~f = G.fold_vertex (fun x acc -> f acc x) g init
end

module E_foldable = struct
  type t = G.t

  type elem = G.E.t

  let fold_left g ~init ~f = G.fold_edges_e (fun x acc -> f acc x) g init
end

module Refinement = struct
  type t = { context : Args.t; splits : (State.t * Abs.t list) list }
  [@@deriving sexp_of]

  let pp fmt x = Sexp.pp_hum fmt @@ [%sexp_of: t] x
end

let cone graph target_node separator =
  let in_separator =
    let sep = Set.of_list (module Node) separator in
    Set.mem sep
  in

  let graph' = G.create () in
  let work = Queue.create () in
  Queue.enqueue work target_node;
  let rec loop () =
    match Queue.dequeue work with
    | Some v ->
        G.add_vertex graph' v;
        let succ = G.succ_e graph v in
        (* Add edges to the filtered graph. *)
        List.iter succ ~f:(G.add_edge_e graph');
        (* Add new nodes to the queue. *)
        if not (in_separator v) then
          List.iter succ ~f:(fun (_, _, v') -> Queue.enqueue work v');
        loop ()
    | None -> ()
  in
  loop ();
  graph'

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
    edge_vars : String_id.t Map.M(E).t;
    var_edges : E.t Map.M(String_id).t;
    state_vars : Symb.t Map.M(State).t;
    arg_vars : Symb.t Map.M(Args).t;
  }

  let make_bit_vars prefix =
    List.init (Lazy.force Global.n_bits) ~f:(fun b ->
        Smt.fresh_decl ~prefix:(prefix b) ())
    |> Smt.all

  let make graph =
    let open Smt.Let_syntax in
    let%bind edge_vars =
      let module F = FoldM0 (Smt) (E_foldable) in
      F.fold graph
        ~init:(Map.empty (module E))
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
          let type_ = Node.type_ v in
          Node.match_ v
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

(* let set states b v =
 *   List.map states ~f:(fun s -> Abs.add s b v) |> Or_error.all |> Or_error.ok *)

let refinement_of_model graph separator interpolant forced vars =
  raise_s [%message "unimplemented" [%here]]

(* let refinement_of_model graph separator interpolant forced vars =
 *   let forced = Map.of_alist_exn (module String_id) forced in
 *   let ivars = Smt.Expr.vars interpolant in
 * 
 *   let used_ivars = ref (Set.empty (module String_id)) in
 * 
 *   let get_bits =
 *     List.filter_mapi ~f:(fun i x ->
 *         if Set.mem ivars x then (
 *           used_ivars := Set.add !used_ivars x;
 *           Some (i, Map.find_exn forced x) )
 *         else None)
 *   in
 * 
 *   let open Option.Let_syntax in
 *   let refinement =
 *     List.filter_map separator ~f:Node.to_args
 *     |> List.dedup_and_sort ~compare:[%compare: Args.t]
 *     (\* Only care about args nodes with refined output bits. *\)
 *     |> List.filter_map ~f:(fun arg_v ->
 *            let refined_out_bits =
 *              let bits = Map.find_exn vars.arg_vars arg_v |> get_bits in
 *              let bits' =
 *                G.pred graph (Node.of_args arg_v)
 *                |> List.map ~f:Node.to_state_exn
 *                |> List.map ~f:(Map.find_exn vars.state_vars)
 *                |> List.map ~f:get_bits |> List.concat
 *              in
 *              normalize_bits (bits @ bits')
 *            in
 *            let no_refined_out_bits = List.is_empty refined_out_bits in
 * 
 *            if no_refined_out_bits then None
 *            else
 *              let state_nodes =
 *                Node.of_args arg_v |> G.pred graph
 *                |> List.map ~f:Node.to_state_exn
 *              in
 *              let input_forced =
 *                match Args.op arg_v with
 *                | Input in_ -> fun bit -> Some in_.(bit)
 *                | _ -> fun _ -> None
 *              in
 * 
 *              let splits =
 *                List.map state_nodes ~f:(fun v ->
 *                    let state = State.state v in
 *                    (\* For each refined bit, generate a pair of refined states.
 *                       If the bit contradicts a bit that is already refined, the
 *                       state will be pruned. *\)
 *                    let refined_states =
 *                      List.fold refined_out_bits
 *                        ~init:(Some [ state ])
 *                        ~f:(fun states (bit, forced) ->
 *                          let%bind states = states in
 *                          match Option.first_some (input_forced bit) forced with
 *                          | Some value -> set states bit value
 *                          | None ->
 *                              let%bind s = set states bit true in
 *                              let%bind s' = set states bit false in
 *                              return (s @ s'))
 *                    in
 *                    (v, Option.value refined_states ~default:[]))
 *              in
 * 
 *              Some { context = arg_v; splits })
 *   in
 * 
 *   [%test_result: Set.M(String_id).t] ~expect:ivars !used_ivars;
 * 
 *   let edges =
 *     List.concat_map refinement ~f:(function r ->
 *         G.pred_e graph (Node.of_args r.context))
 *     |> Set.of_list (module E)
 *   in
 * 
 *   Dump.dump_detailed ~suffix:"after-refine"
 *     ~separator:(List.mem separator ~equal:[%equal: Node.t])
 *     ~refinement:(Set.mem edges) graph;
 *   Fmt.epr "Refinement: %a@." Fmt.Dump.(list Refinement.pp) refinement;
 *   if List.is_empty refinement then failwith "No-op refinement" else refinement *)

let assert_group_var g v = Smt.(Interpolant.assert_group ~group:g (var v))

let vars_to_exprs = List.map ~f:Smt.var

let find_edge_vars vars = List.map ~f:(Map.find_exn vars.edge_vars)

let pred_selected vars g v =
  G.pred_e g v |> find_edge_vars vars |> vars_to_exprs

let succ_selected vars g v =
  G.succ_e g v |> find_edge_vars vars |> vars_to_exprs

let filter g ~f = G.fold_vertex (fun v vs -> if f v then v :: vs else vs) g []

let to_list g = G.fold_vertex (fun v vs -> v :: vs) g []

let assert_input_states_contained group vars graph =
  let open Smt in
  Map.to_alist vars.state_vars
  |> List.filter ~f:(fun (v, _) ->
         List.is_empty (G.succ graph @@ Node.of_state v))
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
  let op = Args.op v in
  let incoming_edges = G.succ_e graph (Node.of_args v) in

  let incoming_states =
    List.map incoming_edges ~f:(fun (_, n, v) -> (Node.to_state_exn v, n))
    |> List.sort ~compare:(fun (_, n) (_, n') -> [%compare: int] n n')
    |> List.map ~f:(fun (v, _) -> Map.find_exn vars.state_vars v)
  in

  let out = Map.find_exn vars.arg_vars v in
  let name = Fmt.str "semantics-%a-%d" Op.pp op (Args.id v) in
  fresh_defn ~prefix:name Symb.(eval op incoming_states = out)
  >>= assert_group_var group

let assert_args_output graph group =
  let open Smt.Let_syntax in
  let target_node =
    match filter graph ~f:(fun v -> G.in_degree graph v = 0) with
    | [ v ] -> Node.to_args_exn v
    | vs -> raise_s [%message "Expected one target node." (vs : Node.t list)]
  in

  let%bind vars = Vars.make graph in
  let%bind () = assert_input_states_contained group vars graph in

  let%bind () =
    let module F = FoldM0 (Smt) (V_foldable) in
    List.map (to_list graph) ~f:(fun v ->
        Node.match_ v
          ~state:(fun v ->
            let%bind () =
              assert_selected_state_selects_input group vars graph v
            in
            assert_state_semantics group vars graph v)
          ~args:(fun v ->
            let%bind () =
              assert_selected_args_selects_inputs group vars graph v
            in
            assert_args_semantics group vars graph v))
    |> Smt.all_unit
  in
  return @@ Map.find_exn vars.arg_vars target_node

let synth_constrs graph target_node expected_output separator =
  let open Smt.Let_syntax in
  let%bind lo_group = Smt.Interpolant.Group.create
  and hi_group = Smt.Interpolant.Group.create in

  let top_graph =
    let g = G.copy graph in
    List.iter separator ~f:(G.iter_succ_e (G.remove_edge_e g) g);
    cone g (Node.of_state target_node) separator
  in
  Dump.dump_detailed ~suffix:"top-graph"
    ~separator:(List.mem separator ~equal:[%equal: Node.t])
    top_graph;
  let%bind vars = Vars.make top_graph in

  let%bind vars =
    let module F = FoldM (Smt) (List) in
    F.fold separator ~init:vars ~f:(fun vars v ->
        let%bind local_arg_vars =
          let local_graph = cone graph v [] in
          Dump.dump_detailed ~suffix:"local-graph"
            ~separator:(List.mem separator ~equal:[%equal: Node.t])
            local_graph;
          assert_args_output local_graph lo_group
        in
        return
          {
            vars with
            arg_vars =
              Map.set vars.arg_vars ~key:(Node.to_args_exn v)
                ~data:local_arg_vars;
          })
  in

  let%bind () =
    assert_output_state_contained hi_group vars target_node expected_output
  in
  let%bind () =
    let module F = FoldM0 (Smt) (V_foldable) in
    F.iter top_graph ~f:(fun v ->
        if List.mem separator ~equal:[%compare.equal: Node.t] v then return ()
        else
          Node.match_ v
            ~state:(fun v ->
              let%bind () =
                assert_selected_state_selects_input hi_group vars top_graph v
              in
              assert_state_semantics hi_group vars top_graph v)
            ~args:(fun v ->
              let%bind () =
                assert_selected_args_selects_inputs hi_group vars top_graph v
              in
              assert_args_semantics hi_group vars top_graph v))
  in
  return (vars, lo_group)

let get_refinement state target_node expected_output separator =
  (* Select the subset of the graph that can reach the target. *)
  let graph = cone state (Node.of_state target_node) separator in
  let separator = List.filter separator ~f:(G.mem_vertex graph) in

  Dump.dump_detailed ~suffix:"before-refine"
    ~separator:(List.mem separator ~equal:[%equal: Node.t])
    graph;
  let open Smt in
  let open Let_syntax in
  let process_interpolant state vars interpolant =
    let interpolant = ok_exn interpolant in
    Fmt.epr "Interpolant: %a@." Expr.pp interpolant;
    refinement_of_model graph separator interpolant
      (forced_bits interpolant state)
      vars
  in

  let process_model vars model =
    List.filter_map model ~f:(fun (e, is_selected) ->
        if is_selected then Map.find vars.var_edges e else None)
    |> Set.of_list (module E)
  in

  let get_interpolant =
    let%bind vars, sep_group =
      synth_constrs graph target_node expected_output separator
    in
    let%bind ret = Smt.get_interpolant_or_model [ sep_group ] in
    return (vars, ret)
  in

  let (vars, ret), state = Smt.run get_interpolant in
  Either.map
    ~first:(process_interpolant state vars)
    ~second:(process_model vars) ret
