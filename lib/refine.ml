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
    edge_vars : String_id.t Map.M(UG.E).t;
    var_edges : UG.E.t Map.M(String_id).t;
    state_vars : Symb.t Map.M(UG.V).t;
    arg_vars : Symb.t Map.M(UG.V).t;
  }

  let make_bit_vars params prefix =
    List.init params.n_bits ~f:(fun b -> Smt.fresh_decl ~prefix:(prefix b) ())
    |> Smt.all

  let make ss graph vertex_rel =
    let open Smt.Let_syntax in
    let%bind edge_vars =
      let module F = FoldM0 (Smt) (E_foldable) in
      F.fold graph
        ~init:(Map.empty (module UG.E))
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
        ~init:(Map.empty (module UG.V), Map.empty (module UG.V))
        ~f:(fun (ms, ma) v ->
          let node = vertex_rel.Unshare.One_to_many.backward v in
          Node.match_ node
            ~args:(fun args_v ->
              let%map vars =
                Symb.create (params ss)
                  ~prefix:(sprintf "a%d_b%d_" (Args.id args_v))
                @@ Args.output_type ss args_v
              in
              (ms, Map.set ma ~key:v ~data:vars))
            ~state:(fun state_v ->
              let%map vars =
                Symb.of_abs (params ss)
                  ~prefix:(sprintf "s%d_b%d_" (State.id state_v))
                @@ State.state ss state_v
              in
              (Map.set ms ~key:v ~data:vars, ma)))
    in

    return { edge_vars; var_edges; state_vars; arg_vars }
end

open Vars

let inter_partition p p' =
  Set.to_sequence p
  |> Sequence.concat_map ~f:(fun v ->
         Set.to_sequence p'
         |> Sequence.map ~f:(Abs.meet v)
         |> Sequence.filter ~f:(Fun.negate Abs.is_bottom))
  |> Sequence.to_list
  |> Set.of_list (module Abs)

let set_concat_map m s ~f =
  Set.to_sequence s |> Sequence.map ~f
  |> Sequence.reduce ~f:Set.union
  |> Option.value ~default:(Set.empty m)

let[@landmark "process-interpolant"] refinement_of_interpolant ss graph rel
    interpolant lower_constr vars =
  let interpolant_vars = Smt.Expr.vars interpolant in
  let refinement =
    lower_constr
    (* Only consider refining symbolic vars that are mentioned in the
       interpolant *)
    |> List.map ~f:(fun (state_v, smt_state) ->
           (state_v, Map.find_exn vars.state_vars state_v, smt_state))
    |> List.filter ~f:(fun (_, symb, _) ->
           Set.inter (Symb.vars symb) interpolant_vars |> Set.is_empty |> not)
    |> List.concat_map ~f:(fun (state_v, symb, smt_state) ->
           let old_state =
             State.state ss @@ Node.to_state_exn @@ rel.backward state_v
           in

           print_s [%message "learning refinement" (old_state : Abs.t)];

           let new_states =
             Symb.refine (params ss) interpolant smt_state old_state symb
           in
           [%test_pred: Set.M(Abs).t] (fun s -> not (Set.is_empty s)) new_states;

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

let assert_group_var v = Smt.(assert_ (var v))

let vars_to_exprs = List.map ~f:Smt.var

let find_edge_vars vars = List.map ~f:(Map.find_exn vars.edge_vars)

let pred_selected vars g v =
  UG.pred_e g v |> find_edge_vars vars |> vars_to_exprs

let succ_selected vars g v =
  UG.succ_e g v |> find_edge_vars vars |> vars_to_exprs

let filter g ~f = G.fold_vertex (fun v vs -> if f v then v :: vs else vs) g []

let to_list g = G.fold_vertex (fun v vs -> v :: vs) g []

let assert_input_states_contained ss vars rel =
  let open Smt in
  Map.to_alist vars.state_vars
  |> List.map ~f:(fun (v, var) ->
         let state_v = Node.to_state_exn @@ rel.backward v in
         let state = State.state ss state_v in
         assert_ (Symb.contained (params ss) var ~by:state))
  |> all_unit

let assert_output_state_contained params vars target_node expected =
  let open Smt in
  let var = Map.find_exn vars.state_vars target_node in
  fresh_defn ~prefix:"correct-output"
    (Symb.contained params var ~by:(Abs.lift expected))
  >>= assert_group_var

let assert_selected_state_selects_input vars graph rel v =
  let open Smt in
  let id =
    let state_v = Node.to_state_exn @@ rel.backward v in
    State.id state_v
  in
  let parent_select =
    if UG.in_degree graph v > 0 then or_ @@ pred_selected vars graph v
    else true_
  and child_select =
    if UG.out_degree graph v > 0 then exactly_one @@ succ_selected vars graph v
    else true_
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

let assert_state_semantics vars graph rel v =
  let open Smt in
  let id =
    let state_v = Node.to_state_exn @@ rel.backward v in
    State.id state_v
  in
  let state_vars = Map.find_exn vars.state_vars v in
  let body =
    UG.succ_e graph v
    |> List.map ~f:(fun ((_, _, v) as e) ->
           let args_vars = Map.find_exn vars.arg_vars v in
           let is_selected = Map.find_exn vars.edge_vars e |> var in
           is_selected => Symb.(args_vars = state_vars))
    |> and_
  and name = sprintf "state-%d-semantics" id in
  fresh_defn ~prefix:name body >>= assert_group_var

let assert_args_semantics ss vars graph rel v =
  let open Smt.Let_syntax in
  let op, id =
    let args_v = Node.to_args_exn @@ rel.backward v in
    (Args.op ss args_v, Args.id args_v)
  in

  let incoming_states =
    UG.succ_e graph v
    |> List.map ~f:(fun (_, n, v) -> (v, n))
    |> List.sort ~compare:(fun (_, n) (_, n') -> [%compare: int] n n')
    |> List.map ~f:(fun (v, _) -> Map.find_exn vars.state_vars v)
  in

  let out = Map.find_exn vars.arg_vars v in

  if Op.arity op <> List.length incoming_states then
    raise_s
      [%message "unexpected args" (op : _ Op.t) (incoming_states : Symb.t list)];

  Smt.with_comment_block
    ~name:(Fmt.str "semantics %a %d" Op.pp op id)
    ~descr:[%message (op : Offset.t Op.t)]
    (let%bind eval_result = Symb.eval (params ss) op incoming_states in
     Symb.(eval_result = out) |> Smt.assert_)

let in_separator s v =
  Node.match_ ~args:(Fun.const false)
    ~state:(List.mem s ~equal:[%compare.equal: State.t])
    v

module FV = FoldM0 (Smt) (V_foldable)
module F = FoldM2 (Smt) (Set)
module FL = FoldM (Smt) (List)

let[@landmark "find-interpolant"] run_solver ss graph rel target_node
    expected_output separator =
  let open Smt.Let_syntax in
  (* There should only be one target even after unsharing *)
  let target_node =
    Node.of_state target_node |> rel.forward |> Sequence.hd_exn
  in

  let top_graph =
    let g = UG.copy graph in

    (* Remove separator dependency edges. *)
    Set.iter separator ~f:(UG.iter_succ_e (UG.remove_edge_e g) g);

    UCone.cone g [ target_node ]
  in

  let separator = Set.filter separator ~f:(UG.mem_vertex top_graph) in

  [%test_pred: Set.M(UG.V).t] ~message:"separator not in top graph"
    (Set.for_all ~f:(UG.mem_vertex top_graph))
    separator;

  let assert_graph_constraints vars g rel =
    let%bind () = assert_input_states_contained ss vars rel in
    FV.iter g ~f:(fun v ->
        Node.match_ (rel.backward v)
          ~state:(fun _ ->
            let%bind () = assert_selected_state_selects_input vars g rel v in
            assert_state_semantics vars g rel v)
          ~args:(fun _ ->
            let%bind () = assert_selected_args_selects_inputs vars g rel v in
            assert_args_semantics ss vars g rel v))
  in

  let hi_group = 0 and lo_group = 1 in

  let high_constr vars =
    let%bind () = assert_input_states_contained ss vars rel in
    let%bind () =
      assert_output_state_contained (params ss) vars target_node expected_output
    in
    assert_graph_constraints vars top_graph rel
  in

  let lower_constrs vars =
    Set.to_list separator
    |> List.map ~f:(fun v ->
           let local_graph = UCone.cone graph [ v ] in

           let local_smt =
             let%bind () = Smt.Interpolant.set_group lo_group in
             let%bind local_vars =
               Smt.with_comment_block ~name:"vars"
                 (let%map vs = Vars.make ss local_graph rel in
                  {
                    vs with
                    state_vars =
                      Map.set vs.state_vars ~key:v
                        ~data:(Map.find_exn vars.state_vars v);
                  })
             in
             assert_graph_constraints local_vars local_graph rel
           in

           (v, local_smt))
  in

  let vars, vars_state =
    (let%bind () = Smt.Interpolant.set_group hi_group in
     Vars.make ss top_graph rel)
    |> Smt.run
  in

  let upper_ok =
    ( (let%bind () = high_constr vars in
       Smt.check_sat)
    |> Smt.eval_with_state vars_state )
    [@landmark "check-separator-upper"]
  in

  let lower_ok =
    ( (let%bind () = FL.iter (lower_constrs vars) ~f:(fun (_, c) -> c) in
       Smt.check_sat)
    |> Smt.eval_with_state vars_state )
    [@landmark "check-separator-lower"]
  in

  print_s
    [%message (Set.length separator : int) (lower_ok : bool) (upper_ok : bool)];

  if upper_ok && lower_ok then
    let interpolant_or_model =
      ( (print_s [%message "seeking interpolant"];

         (let%bind () = high_constr vars in
          let%bind () = Smt.Interpolant.set_group lo_group in
          let%bind () = FL.iter (lower_constrs vars) ~f:(fun (_, c) -> c) in
          Smt.get_interpolant_or_model [ lo_group ])
         |> Smt.eval_with_state vars_state)
      [@landmark "get-interpolant"] )
    in

    match interpolant_or_model with
    | First interpolant -> (
        let interpolant = Or_error.ok_exn interpolant in
        match interpolant with
        | Smt.Expr.Bool _ -> Second ([], vars)
        | _ ->
            let lower_constr_states =
              List.map (lower_constrs vars) ~f:(fun (v, c) ->
                  let (), state = Smt.with_state vars_state c in
                  (v, state))
            in
            First (interpolant, lower_constr_states, vars) )
    | Second model -> Second (model, vars)
  else Second ([], vars)

let process_interpolant params graph rel (interpolant, lower_constr, vars) =
  Fmt.epr "Interpolant: %a@." Smt.Expr.pp interpolant;

  [%test_pred: Set.M(Smt.Var).t] ~message:"interpolant vars are not all states"
    (Set.for_all ~f:(fun v -> Char.((String_id.to_string v).[0] = 's')))
    (Smt.Expr.vars interpolant);

  refinement_of_interpolant params graph rel interpolant lower_constr vars

let rec extract_program ss graph rel selected_edges target =
  let args =
    UG.succ_e graph target
    |> List.filter ~f:(Set.mem selected_edges)
    |> List.map ~f:(fun (_, _, v) -> v)
  in
  match args with
  | [ a ] ->
      `Apply
        ( Args.op ss @@ Node.to_args_exn @@ rel.backward a,
          UG.succ_e graph a
          |> List.sort ~compare:(fun (_, i, _) (_, j, _) -> [%compare: int] i j)
          |> List.map ~f:(fun (_, _, v) ->
                 extract_program ss graph rel selected_edges v) )
  | args ->
      let args =
        List.map args ~f:(fun v -> Node.to_args_exn @@ rel.backward v)
      in
      raise_s [%message "expected one argument" (args : Args.t list)]

let process_model ss graph rel target (model, vars) =
  let selected_edges =
    List.filter_map model ~f:(fun (e, is_selected) ->
        if is_selected then Map.find vars.var_edges e else None)
    |> Set.of_list (module UG.E)
  in
  extract_program ss graph rel selected_edges target

let[@landmark "refine"] get_refinement ss target_node =
  (* Select the subset of the graph that can reach the target *)
  let shared_graph =
    let module C = Cone.Make (Search_state.G) in
    C.cone (graph ss) [ Node.of_state target_node ]
  in

  Search_state.dump_detailed_graph ~suffix:"before-unsharing" ss shared_graph;

  (* Remove sharing in the graph subset *)
  let graph, rel = U.unshare shared_graph in
  let top =
    Option.value_exn ~message:"graph does not have a greatest element"
      (UFold.V.find graph ~f:(fun v -> UG.in_degree graph v = 0))
  in

  let size = G.nb_vertex shared_graph
  and unshared_size = UG.nb_vertex graph
  and top_out_degree = UG.out_degree graph top in
  print_s
    [%message
      "conflict graph" (size : int) (unshared_size : int) (top_out_degree : int)];

  dump_detailed ss ~suffix:"after-unsharing" graph rel;

  let expected_output = Conc.bool_vector (params ss).bench.output in

  let m_refinement =
    let separators = USeparator.simple graph top in

    [%test_pred: Set.M(UG.V).t Sequence.t]
      ~message:"separator isn't all state nodes"
      (Sequence.for_all
         ~f:
           (Set.for_all ~f:(fun v ->
                rel.backward v
                |> Node.match_ ~args:(Fun.const false) ~state:(Fun.const true))))
      separators;

    Sequence.find_map separators ~f:(fun separator ->
        match run_solver ss graph rel target_node expected_output separator with
        | First x -> process_interpolant ss graph rel x |> Option.some
        | _ -> None)
  in

  match m_refinement with
  | Some r -> First r
  | None -> (
      match
        run_solver ss graph rel target_node expected_output
          (Set.empty (module UG.V))
      with
      | First _ -> failwith "expected model"
      | Second x -> Second (process_model ss graph rel top x) )
