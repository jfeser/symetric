open Params
open Foldm

module Make
    (Lang : Lang_intf.S)
    (Search_state : Search_state_intf.S
                      with type op = Lang.Op.t
                       and type abs = Lang.Abs.t
                       and type params = Lang.params) =
struct
  open Lang
  open Search_state

  module U =
    Unshare.Make
      (G)
      (struct
        let kind = Node.match_ ~args:(fun _ -> `Args) ~state:(fun _ -> `State)
      end)

  open Unshare.One_to_many
  module UG = U.G_replicated
  module UFold = Graph_ext.Folds (U.G_replicated)
  module UCone = Cone.Make (U.G_replicated)
  module USeparator = Separator.Make (U.G_replicated)

  let dump_detailed ~suffix ?separator ss graph rel =
    let (module Attr) = attr ss in
    let module D =
      Dump.Make
        (U.G_replicated)
        (struct
          let vertex_name v = Fmt.str "%d" @@ U.id v

          let vertex_attributes v = Attr.vertex_attributes @@ rel.backward v
        end)
    in
    let open D in
    dump_detailed ~suffix ?separator (params ss) graph

  module V_foldable = struct
    type t = UG.t

    type elem = UG.V.t

    let fold g ~init ~f = UG.fold_vertex (fun x acc -> f acc x) g init
  end

  module E_foldable = struct
    type t = UG.t

    type elem = UG.E.t

    let fold g ~init ~f = UG.fold_edges_e (fun x acc -> f acc x) g init
  end

  module Refinement = struct
    type s = { old : Set.M(Abs).t; new_ : Set.M(Abs).t } [@@deriving sexp]

    type t = s Map.M(Args).t [@@deriving sexp_of]

    let _union { old = o; new_ = n } { old = o'; new_ = n' } =
      { old = Set.union o o'; new_ = Set.union n n' }
  end

  module Vars = struct
    type t = {
      edge_vars : String_id.t Map.M(UG.E).t;
      state_vars : Symb.t Map.M(UG.V).t;
    }

    let make ss graph rel target expected =
      let open Smt.Let_syntax in
      let%bind edge_vars =
        let module F = FoldM0 (Smt) (E_foldable) in
        F.fold graph
          ~init:(Map.empty (module UG.E))
          ~f:(fun m e ->
            let%map decl = Smt.fresh_decl ~prefix:"e" () in
            Map.set m ~key:e ~data:decl)
      in

      let%bind state_vars =
        let module F = FoldM0 (Smt) (V_foldable) in
        F.fold graph
          ~init:(Map.empty (module UG.V))
          ~f:(fun ms v ->
            let node = rel.backward v in
            Node.match_ node
              ~args:(fun _ -> return ms)
              ~state:(fun state_v ->
                let%map vars =
                  if List.mem ~equal:[%compare.equal: UG.V.t] target v then
                    return @@ Symb.of_conc (params ss) expected
                  else Abs.to_symb (params ss) @@ State.state ss state_v
                in
                Map.set ms ~key:v ~data:vars))
      in

      return { edge_vars; state_vars }
  end

  open Vars

  let assert_group_var v = Smt.(assert_ (var v))

  let vars_to_exprs = List.map ~f:Smt.var

  let find_edge_vars vars = List.map ~f:(Map.find_exn vars.edge_vars)

  let pred_selected vars g v =
    UG.pred_e g v |> find_edge_vars vars |> vars_to_exprs

  let succ_selected vars g v =
    UG.succ_e g v |> find_edge_vars vars |> vars_to_exprs

  let assert_selected_state_selects_input vars graph rel v =
    let open Smt in
    let open Smt.Let_syntax in
    let id =
      let state_v = Node.to_state_exn @@ rel.backward v in
      State.id state_v
    in
    let parent_select =
      if UG.in_degree graph v > 0 then or_ @@ pred_selected vars graph v
      else true_
    in
    let%bind child_select =
      if UG.out_degree graph v > 0 then
        exactly_one @@ succ_selected vars graph v
      else return true_
    in
    let body = parent_select => child_select
    and name = sprintf "state-%d-deps" id in
    fresh_defn ~prefix:name body >>= assert_group_var

  let assert_selected_args_selects_inputs vars graph rel v =
    let open Smt in
    (* Args with no dependencies don't generate a constraint. *)
    if List.is_empty @@ UG.succ graph v then return ()
    else
      let id =
        let args_v = Node.to_args_exn @@ rel.backward v in
        Args.id args_v
      in
      let parent_select =
        if UG.in_degree graph v > 0 then or_ @@ pred_selected vars graph v
        else true_
      and child_select = succ_selected vars graph v in
      let body = parent_select => and_ child_select
      and name = sprintf "args-%d-deps" id in
      fresh_defn ~prefix:name body >>= assert_group_var

  let assert_state_semantics ss vars graph rel v =
    let open Smt.Let_syntax in
    let id =
      let state_v = Node.to_state_exn @@ rel.backward v in
      State.id state_v
    in
    let state_vars = Map.find_exn vars.state_vars v in
    let%bind body =
      UG.succ_e graph v
      |> List.map ~f:(fun ((_, _, v) as e) ->
             let op = Args.op ss @@ Node.to_args_exn @@ rel.backward v in

             let is_selected = Map.find_exn vars.edge_vars e |> Smt.var in

             let incoming_states =
               UG.succ_e graph v
               |> List.map ~f:(fun (_, n, v) -> (v, n))
               |> List.sort ~compare:(fun (_, n) (_, n') ->
                      [%compare: int] n n')
               |> List.map ~f:(fun (v, _) -> Map.find_exn vars.state_vars v)
             in

             let%bind arg_out = Symb.eval (params ss) op incoming_states in

             return Smt.(is_selected => Symb.equals arg_out state_vars))
      |> Smt.all
    in
    let name = sprintf "state-%d-semantics" id in
    let%bind defn = Smt.fresh_defn ~prefix:name (Smt.and_ body) in
    assert_group_var defn

  module FV = FoldM0 (Smt) (V_foldable)
  module F = FoldM2 (Smt) (Set)
  module FL = FoldM (Smt) (List)

  (** Return the subset of `graph` that is reachable from `target`. *)
  let rand_cone graph targets =
    let graph' = G.create () in
    let work = Queue.create () in
    Queue.enqueue_all work @@ List.filter ~f:(G.mem_vertex graph) targets;
    let rec loop () =
      match Queue.dequeue work with
      | Some v ->
          G.add_vertex graph' v;

          let succ =
            let succ = G.succ_e graph v in

            if Node.is_state v then List.take (List.permute succ) 1 else succ
          in

          (* Add edges to the filtered graph. *)
          List.iter succ ~f:(G.add_edge_e graph');
          (* Add new nodes to the queue. *)
          List.iter succ ~f:(fun (_, _, v') -> Queue.enqueue work v');
          loop ()
      | None -> ()
    in
    loop ();
    graph'

  let assert_graph_constraints ss vars g rel =
    let open Smt.Let_syntax in
    FV.iter g ~f:(fun v ->
        Node.match_ (rel.backward v)
          ~state:(fun _ ->
            let%bind () = assert_selected_state_selects_input vars g rel v in
            assert_state_semantics ss vars g rel v)
          ~args:(fun _ -> assert_selected_args_selects_inputs vars g rel v))

  let rec extract_program ss graph rel selected_edges target =
    let inputs =
      UG.succ_e graph target
      |> List.filter ~f:(Set.mem selected_edges)
      |> List.map ~f:(fun (_, _, v) -> v)
    in
    match inputs with
    | [ a ] ->
        let op = Args.op ss @@ Node.to_args_exn @@ rel.backward a in
        let args =
          UG.succ_e graph a
          |> List.sort ~compare:(fun (_, i, _) (_, j, _) -> [%compare: int] i j)
          |> List.map ~f:(fun (_, _, v) ->
                 extract_program ss graph rel selected_edges v)
          |> Option.all
        in
        Option.map args ~f:(fun args -> Program.Apply (op, args))
    | _ -> None

  let process_model ss graph rel target vars model =
    let var_edges =
      Map.to_alist vars.edge_vars
      |> List.map ~f:Tuple.T2.swap
      |> Map.of_alist_exn (module String_id)
    in
    let selected_edges =
      List.filter_map model ~f:(fun (e, is_selected) ->
          if is_selected then Map.find var_edges e else None)
      |> Set.of_list (module UG.E)
    in
    List.find_map_exn target ~f:(extract_program ss graph rel selected_edges)

  let[@landmark "find-program"] find_program ss graph rel target expected_output
      =
    let open Smt.Let_syntax in
    let vars, m_model =
      (let%bind vars = Vars.make ss graph rel target expected_output in
       let%bind () = assert_graph_constraints ss vars graph rel in
       let%bind model = Smt.get_model in
       return (vars, model))
      |> Smt.eval
    in
    Option.map m_model ~f:(process_model ss graph rel target vars)

  let[@landmark "find-interpolant-with-separator"] find_interpolant_with_separator
      ss graph rel target_nodes expected_output separator =
    let top_graph =
      let g = UG.copy graph in

      (* Remove separator dependency edges. *)
      Set.iter separator ~f:(UG.iter_succ_e (UG.remove_edge_e g) g);

      UCone.cone g target_nodes
    in

    let separator = Set.filter separator ~f:(UG.mem_vertex top_graph) in
    let hi_group = 0 and lo_group = 1 in

    let high_constr vars = assert_graph_constraints ss vars top_graph rel in

    let lower_constrs vars =
      List.map (Set.to_list separator) ~f:(fun v ->
          let local_graph = UCone.cone graph [ v ] in

          let local_smt =
            let open Smt.Let_syntax in
            let%bind () = Smt.Interpolant.set_group lo_group in
            let%bind local_vars =
              Smt.with_comment_block ~name:"vars"
              @@ let%map vs =
                   Vars.make ss local_graph rel target_nodes expected_output
                 in
                 {
                   vs with
                   state_vars =
                     Map.set vs.state_vars ~key:v
                       ~data:(Map.find_exn vars.state_vars v);
                 }
            in
            assert_graph_constraints ss local_vars local_graph rel
          in

          (v, local_smt))
    in

    let vars, vars_state =
      let open Smt.Let_syntax in
      (let%bind () = Smt.Interpolant.set_group hi_group in
       Vars.make ss top_graph rel target_nodes expected_output)
      |> Smt.run
    in

    let upper_ok =
      let open Smt.Let_syntax in
      ( (let%bind () = high_constr vars in
         Smt.check_sat)
      |> Smt.eval_with_state vars_state )
      [@landmark "check-separator-upper"]
    in

    print_s [%message (Set.length separator : int) (upper_ok : bool)];

    if upper_ok then
      let interpolant =
        ((let open Smt.Let_syntax in
         (let%bind () = high_constr vars in
          let%bind () = Smt.Interpolant.set_group lo_group in
          let%bind () = FL.iter (lower_constrs vars) ~f:(fun (_, c) -> c) in
          Smt.get_interpolant [ lo_group ])
         |> Smt.eval_with_state vars_state) [@landmark "get-interpolant"])
        |> Option.value_exn ~here:[%here] ~message:"expected interpolant"
      in
      let lower_constr_states =
        List.map (lower_constrs vars) ~f:(fun (v, c) ->
            let (), state = Smt.with_state vars_state c in
            (v, state))
      in
      Some (interpolant, lower_constr_states, vars)
    else None

  let find_refinement ss graph rel (interpolant, lower_constr, vars) =
    let ivars = Smt.Expr.vars interpolant in
    let refinement =
      lower_constr
      (* Only consider refining symbolic vars that are mentioned in the
         interpolant *)
      |> List.map ~f:(fun (state_v, smt_state) ->
             (state_v, Map.find_exn vars.state_vars state_v, smt_state))
      |> List.filter ~f:(fun (_, symb, _) ->
             Set.inter (Symb.vars symb) ivars |> Set.is_empty |> not)
      |> List.concat_map ~f:(fun (state_v, symb, smt_state) ->
             let old_state =
               State.state ss @@ Node.to_state_exn @@ rel.backward state_v
             in
             let new_states =
               Symb.refine (params ss) interpolant smt_state old_state symb
               |> Set.to_list |> Abs.roots
               |> Set.of_list (module Abs)
             in
             [%test_pred: Set.M(Abs).t]
               (fun s -> not (Set.is_empty s))
               new_states;

             let args_vs =
               UG.succ graph state_v
               |> List.map ~f:(fun v -> Node.to_args_exn @@ rel.backward v)
             in

             List.map args_vs ~f:(fun args_v ->
                 ( args_v,
                   {
                     Refinement.old = Set.singleton (module Abs) old_state;
                     new_ = new_states;
                   } )))
      |> Map.of_alist_reduce
           (module Args)
           ~f:(fun { old = o; new_ = n } { old = o'; new_ = n' } ->
             { old = Set.union o o'; new_ = Set.union n n' })
    in

    print_s [%message "refinement" (refinement : Refinement.t)];

    refinement

  let[@landmark "find-interpolant"] find_interpolant ss graph rel target
      expected =
    let separators = USeparator.random graph target in

    [%test_pred: Set.M(UG.V).t Sequence.t]
      ~message:"separator isn't all state nodes"
      (Sequence.for_all
         ~f:
           (Set.for_all ~f:(fun v ->
                rel.backward v
                |> Node.match_ ~args:(Fun.const false) ~state:(Fun.const true))))
      separators;

    Sequence.find_map separators
      ~f:(find_interpolant_with_separator ss graph rel target expected)
    |> Option.value_exn ~here:[%here] ~message:"could not find interpolant"

  let cone ss target_nodes =
    match (params ss).cone with
    | `Full ->
        let module C = Cone.Make (Search_state.G) in
        C.cone (graph ss) @@ List.map ~f:Node.of_state target_nodes
    | `Rand -> rand_cone (graph ss) @@ List.map ~f:Node.of_state target_nodes

  let[@landmark "refine"] refine ss target =
    (* Select the subset of the graph that can reach the target *)
    let shared_graph = cone ss target in

    Search_state.dump_detailed_graph ~suffix:"before-unsharing" ss shared_graph;

    (* Remove sharing in the graph subset *)
    let graph, rel = U.unshare shared_graph in
    let size = G.nb_vertex shared_graph
    and unshared_size = UG.nb_vertex graph in

    print_s [%message "conflict graph" (size : int) (unshared_size : int)];

    dump_detailed ss ~suffix:"after-unsharing" graph rel;

    let target =
      Sequence.of_list target
      |> Sequence.map ~f:Node.of_state
      |> Sequence.concat_map ~f:rel.forward
      |> Sequence.to_list
    in

    let expected = Bench.output (params ss).bench in

    match find_program ss graph rel target expected with
    | Some p -> Second p
    | None ->
        find_interpolant ss graph rel target expected
        |> find_refinement ss graph rel
        |> Either.first

  let summarize = None
end
