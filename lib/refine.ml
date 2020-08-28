open Search_state

module Refinement = struct
  type t = [ `Split of Args.t * int * Abs.t list ] list
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
        let succ = succ_e graph v in
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

let make_vars graph =
  let open Smt.Let_syntax in
  let%bind edge_var_rel =
    E.to_list graph
    |> List.map ~f:(fun e ->
           let%bind decl = Smt.fresh_decl ~prefix:"e" () in
           return (e, decl))
    |> Smt.all
  in

  let edge_vars = edge_var_rel |> Map.of_alist_exn (module E)
  and var_edges =
    List.map ~f:Tuple.T2.swap edge_var_rel |> Map.of_alist_exn (module String_id)
  in

  let%bind state_vars =
    V.filter_map graph ~f:Node.to_state
    |> List.map ~f:(fun (v : State.t) ->
           let%bind vars =
             List.init !Global.n_bits ~f:(fun vec ->
                 match Map.find (State.state v) vec with
                 | Some x -> return @@ Smt.Bool.bool x
                 | None ->
                     let%bind var =
                       Smt.fresh_decl
                         ~prefix:(sprintf "s%d_b%d_" (State.id v) vec)
                         ()
                     in
                     return (Smt.Expr.Var var))
             |> Smt.all
           in
           return (v, vars))
    |> Smt.all
  in
  let state_vars = Map.of_alist_exn (module State) state_vars in

  let%bind arg_vars =
    V.filter_map graph ~f:Node.to_args
    |> List.map ~f:(fun (v : Args.t) ->
           let%bind vars =
             List.init !Global.n_bits ~f:(fun b ->
                 Smt.fresh_decl
                   ~prefix:(sprintf "a%d_b%d_" (Args.id v) b)
                   ())
             |> Smt.all
           in
           return (v, vars))
    |> Smt.all
  in
  let arg_vars = Map.of_alist_exn (module Args) arg_vars in
  return (edge_vars, var_edges, state_vars, arg_vars)

let check_true graph interpolant v state =
  let open Smt.Let_syntax in
  let prob =
    let%bind () = Smt.(assert_ Bool.(not (interpolant => Var v))) in
    Smt.check_sat
  in
  not (Smt.eval_with_state state prob)

let check_false graph interpolant v state =
  let open Smt.Let_syntax in
  let prob =
    let%bind () = Smt.(assert_ Bool.(not (interpolant => not (Var v)))) in
    Smt.check_sat
  in
  not (Smt.eval_with_state state prob)

let forced_bits graph interpolant state =
  let vars = Smt.Expr.vars interpolant |> Set.to_list in
  if !Global.enable_forced_bit_check then
    List.map vars ~f:(fun var ->
        if check_true graph interpolant var state then (var, Some true)
        else if check_false graph interpolant var state then (var, Some false)
        else (var, None))
  else List.map vars ~f:(fun v -> (v, None))

let refinement_of_model graph separator interpolant forced state_vars
    arg_out_vars =
  let forced = Map.of_alist_exn (module String_id) forced in
  let ivars = Smt.Expr.vars interpolant in

  let get_state_bits =
    List.filter_mapi ~f:(fun i x ->
        match x with
        | Smt.Expr.Var v ->
            if Set.mem ivars v then Some (i, Map.find_exn forced v) else None
        | _ -> None)
  in

  let get_args_bits =
    List.filter_mapi ~f:(fun i x ->
        if Set.mem ivars x then Some (i, Map.find_exn forced x) else None)
  in

  let open Option.Let_syntax in
  Fmt.epr "sep: %a@." Fmt.(Dump.list Node.pp) separator;
  Fmt.epr "args out: %a@."
    Fmt.(Dump.(list @@ pair Node.pp @@ list string))
    ( Map.to_alist arg_out_vars
    |> List.map ~f:(fun (k, v) ->
           (Node.of_args k, List.map ~f:String_id.to_string v)) );
  let refinement =
    separator
    |> List.map ~f:Node.to_args_exn
    |> List.dedup_and_sort ~compare:[%compare: Args.t]
    (* Only care about args nodes with refined output bits. *)
    |> List.filter_map ~f:(fun v ->
           let bits = Map.find_exn arg_out_vars v |> get_args_bits in
           let bits' =
             pred graph (Node.of_args v)
             |> List.map ~f:Node.to_state_exn
             |> List.map ~f:(Map.find_exn state_vars)
             |> List.map ~f:get_state_bits |> List.concat
           in
           let refined_bits = normalize_bits (bits @ bits') in
           if List.is_empty refined_bits then None else Some (v, refined_bits))
    |> List.concat_map ~f:(fun (v, refined_bits) ->
           let (state_nodes : State.t list) =
             pred graph (Node.of_args v) |> List.map ~f:Node.to_state_exn
           in

           Fmt.epr "Refined bits: %a@."
             Fmt.(Dump.(list @@ pair int @@ option bool))
             refined_bits;

           let states = List.map state_nodes ~f:State.state in
           let refined =
             List.fold refined_bits ~init:states ~f:(fun states (bit, forced) ->
                 match forced with
                 | Some value ->
                     List.map states ~f:(fun s -> Abs.set s bit value)
                 | None ->
                     List.concat_map states ~f:(fun s ->
                         [ Abs.set s bit true; Abs.set s bit false ]))
           in

           let changed =
             not
               (Set.equal
                  (Set.of_list (module Abs) states)
                  (Set.of_list (module Abs) refined))
           in

           let ret =
             if changed then
               let cost = List.hd_exn state_nodes |> State.cost in
               [ `Split (v, cost, refined) ]
             else []
           in
           ret)
  in

  let edges =
    List.concat_map refinement ~f:(function
      | `Split (v, _, _) -> pred_e graph (Node.of_args v)
      | `Remove vs ->
          List.concat_map ~f:(fun v -> succ_e graph (Node.of_state v)) vs)
    |> Set.of_list (module E)
  in
  Dump.dump_detailed ~suffix:"after-refine"
    ~separator:(List.mem separator ~equal:[%equal: Node.t])
    ~refinement:(Set.mem edges) graph;

  Fmt.epr "Refinement: %a@." Refinement.pp refinement;
  if List.is_empty refinement then failwith "No-op refinement" else refinement

let get_refinement graph target_node expected_output separator =
  (* Select the subset of the graph that can reach the target. *)
  let graph =
    {
      graph = cone graph (Node.of_state target_node) separator;
      cost_table = [||];
      args_table = Hashtbl.create (module Args_table_key);
      state_table = Hashtbl.create (module State_table_key);
    }
  in
  let separator =
    List.filter separator ~f:(fun v -> G.mem_vertex graph.graph v)
  in

  Dump.dump_detailed ~suffix:"before-refine"
    ~separator:(List.mem separator ~equal:[%equal: Node.t])
    graph;

  let open Smt.Let_syntax in
  let output_constr state_vars =
    (* The output state must have the expected value. *)
    List.map2_exn (Map.find_exn state_vars target_node)
      (Array.to_list expected_output) ~f:(fun x v -> Smt.Bool.(x = bool v))
    |> Smt.Bool.and_
    |> Smt.make_defn "correct-output"
    >>= fun v -> Smt.Interpolant.assert_group (Var v)
  in

  let synth_constrs ~assert_group
      (edge_vars, var_edges, state_vars, arg_out_vars) =
    let assert_group_var g v = assert_group g (Smt.Expr.Var v) in

    let edge_vars = Map.map ~f:(fun v -> Smt.Expr.Var v) edge_vars in
    let arg_out_vars =
      Map.map ~f:(List.map ~f:(fun v -> Smt.Expr.Var v)) arg_out_vars
    in

    (* Each state must be contained by its abstraction *)
    let%bind () =
      Map.to_alist state_vars
      |> List.map ~f:(fun (state_v, vars) ->
             let state = State.state state_v in
             List.filter_mapi vars ~f:(fun b v ->
                 match Map.find state b with
                 | Some x -> Some Smt.Bool.(bool x = v)
                 | None -> None)
             |> Smt.Bool.and_
             |> Smt.make_defn (Fmt.str "state-%d" @@ State.id state_v)
             >>= assert_group_var `A)
      |> Smt.all_unit
    in

    (* Every selected state node must select exactly one dependent args node. *)
    let%bind () =
      V.filter_map graph ~f:Node.to_state
      |> List.map ~f:(fun v ->
             let selected =
               if [%equal: State.t] target_node v then
                 [ Smt.Bool.(bool true) ]
               else
                 pred_e graph (Node.of_state v)
                 |> List.map ~f:(Map.find_exn edge_vars)
             in
             let deps =
               succ_e graph (Node.of_state v)
               |> List.map ~f:(Map.find_exn edge_vars)
             in
             if not (List.is_empty deps) then
               Smt.(
                 make_defn
                   (sprintf "state-%d-deps" (State.id v))
                   Bool.(or_ selected => exactly_one deps)
                 >>= assert_group_var `A)
             else return ())
      |> Smt.all_unit
    in

    (* An arg node with a selected outgoing edge must select all its incoming
       edges. *)
    let%bind () =
      V.filter_map graph ~f:Node.to_args
      |> List.map ~f:(fun v ->
             let outgoing =
               pred_e graph (Node.of_args v)
               |> List.map ~f:(Map.find_exn edge_vars)
             in
             let incoming =
               succ_e graph (Node.of_args v)
               |> List.map ~f:(Map.find_exn edge_vars)
             in
             Smt.Bool.(or_ outgoing => and_ incoming))
      |> Smt.Bool.and_
      |> Smt.make_defn "args-have-all-incoming"
      >>= assert_group_var `A
    in

    let%bind () =
      V.filter_map graph ~f:Node.to_state
      |> List.map ~f:(fun v ->
             let state_in = Map.find_exn state_vars v in
             succ_e graph (Node.of_state v)
             |> List.filter_map ~f:(fun ((_, _, v) as e) ->
                    Node.to_args v
                    |> Option.map ~f:(fun v ->
                           ( Map.find_exn arg_out_vars v,
                             Map.find_exn edge_vars e )))
             |> List.map ~f:(fun (args_out, is_selected) ->
                    Smt.Bool.(
                      is_selected
                      => and_ (List.map2_exn args_out state_in ~f:( = ))))
             |> Smt.Bool.and_)
      |> Smt.Bool.and_
      |> Smt.make_defn "semantics-connected"
      >>= assert_group_var `A
    in

    let%bind () =
      V.filter_map graph ~f:Node.to_args
      |> List.map ~f:(fun v ->
             let incoming_edges = succ_e graph (Node.of_args v) in

             let incoming_states =
               List.map incoming_edges ~f:(fun (_, n, v) ->
                   (Node.to_state_exn v, n))
               |> List.sort ~compare:(fun (_, n) (_, n') ->
                      [%compare: int] n n')
               |> List.map ~f:(fun (v, _) -> v)
               |> List.map ~f:(Map.find_exn state_vars)
             in

             let out = Map.find_exn arg_out_vars v in
             let semantic =
               let open Smt.Bool in
               match (Args.op v, incoming_states) with
               | Union, [ s; s' ] ->
                   List.map3_exn out s s' ~f:(fun x y z -> x = (y || z))
               | Inter, [ s; s' ] ->
                   List.map3_exn out s s' ~f:(fun x y z -> x = (y && z))
               | Sub, [ s; s' ] ->
                   List.map3_exn out s s' ~f:(fun x y z -> x = (y && not z))
               | Input conc, [] ->
                   List.map2_exn out (Array.to_list conc) ~f:(fun x v ->
                       x = bool v)
               | op, states ->
                   Error.create "Unexpected op." (op, states)
                     [%sexp_of: Op.t * Smt.Expr.t list list]
                   |> Error.raise
             in

             let%bind defn =
               Smt.make_defn
                 (Fmt.str "semantics-%a-%d" Op.pp (Args.op v)
                    (Args.id v))
                 (Smt.Bool.and_ semantic)
             in
             if List.mem separator (Node.of_args v) ~equal:[%equal: Node.t] then
               assert_group_var `B defn
             else assert_group_var `A defn)
      |> Smt.all_unit
    in
    return ()
  in

  let ((edge_vars, var_edges, state_vars, arg_out_vars) as vars), state =
    make_vars graph |> Smt.run
  in

  let process_interpolant state_vars arg_out_vars interpolant =
    let interpolant = ok_exn interpolant in
    Fmt.epr "Interpolant: %a@." Smt.Expr.pp interpolant;
    refinement_of_model graph separator interpolant
      (forced_bits graph interpolant state)
      state_vars arg_out_vars
  in

  let process_model var_edges model =
    List.filter_map model ~f:(fun (e, is_selected) ->
        if is_selected then Map.find var_edges e else None)
    |> Set.of_list (module E)
  in

  let get_interpolant =
    let%bind sep_group = Smt.Interpolant.Group.create in
    let%bind () = output_constr state_vars in
    let%bind () =
      synth_constrs
        ~assert_group:(fun g defn ->
          match g with
          | `A -> Smt.Interpolant.assert_group defn
          | `B -> Smt.Interpolant.assert_group ~group:sep_group defn)
        vars
    in
    let%bind ret = Smt.get_interpolant_or_model [ sep_group ] in
    return (arg_out_vars, state_vars, var_edges, ret)
  in

  let arg_out_vars, state_vars, var_edges, ret =
    Smt.eval_with_state state get_interpolant
  in
  Either.map
    ~first:(process_interpolant state_vars arg_out_vars)
    ~second:(process_model var_edges) ret
