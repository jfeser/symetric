[@@@landmark "auto"]

open Ast
open Search_state

module Seq = struct
  include Sequence

  let of_array a = init (Array.length a) ~f:(fun i -> a.(i))
end

module Program = struct
  module T = struct
    type t = [ `Apply of Op.t * t list ] [@@deriving compare, hash, sexp]
  end

  let rec ceval (`Apply (op, args)) = Conc.eval op (List.map args ~f:ceval)

  let rec size (`Apply (_, args)) = 1 + List.sum (module Int) args ~f:size

  include T
  include Comparator.Make (T)
end

let did_change f =
  G.reset_changed ();
  let ret = f () in
  let changed = G.has_changed () in
  (changed, ret)

let until_done f =
  let rec until_done did_work =
    let did_work' = f () in
    if did_work' then until_done did_work' else did_work
  in
  until_done false

let iter_arg graph cost type_ ~f =
  states_of_cost graph cost
  |> List.iter ~f:(fun s ->
         if [%compare.equal: Type.t] (State.type_ s) type_ then f s)

let create_hyper_edge graph cost op args =
  let arg_states = List.map args ~f:State.state in
  let out_state = Abs.eval op arg_states in
  let out_type = Op.ret_type op in
  let state_v_out = State.create out_state cost out_type |> Is_fresh.unwrap in
  insert_hyper_edge_if_not_exists graph args op state_v_out

let fold_range ~init ~f lo hi =
  let rec fold_range acc i =
    if i >= hi then acc
    else
      let acc' = f acc i in
      fold_range acc' (i + 1)
  in
  fold_range init lo

let fill_cost (graph : Search_state.t) ops cost =
  Fmt.epr "Filling cost %d\n" cost;
  let size = nb_vertex graph in
  ( if cost > 1 then
    let arg_cost = cost - 1 in

    List.iter ops ~f:(fun op ->
        let arity = Op.arity op in
        let arg_types = Op.args_type op |> Array.of_list in
        if arity >= arg_cost then
          let module Comp = Combinat.Composition in
          Comp.create ~n:arg_cost ~k:arity
          |> Comp.iter ~f:(fun arg_costs ->
                 let add_hyper_edges =
                   fold_range
                     ~init:(fun args -> create_hyper_edge graph cost op args)
                     ~f:(fun f i args ->
                       let type_ = arg_types.(i) in
                       states_of_cost graph arg_costs.{i}
                       |> List.iter ~f:(fun s ->
                              if [%compare.equal: Type.t] (State.type_ s) type_
                              then f (s :: args)))
                     0 arity
                 in
                 add_hyper_edges [])) );

  (* let arg_cost = cost - 1 in
   * let module Comp = Combinat.Composition in
   * Comp.create ~n:arg_cost ~k:2
   * |> Comp.iter ~f:(fun arg_costs ->
   *        let c = arg_costs.{0} and c' = arg_costs.{1} in
   *        states_of_cost graph c
   *        |> List.iter ~f:(fun (a : State.t) ->
   *               states_of_cost graph c'
   *               |> List.iter ~f:(fun (a' : State.t) ->
   *                      List.iter [ Op.Union; Inter; Sub ] ~f:(fun op ->
   *                          let state =
   *                            let s = State.state a and s' = State.state a' in
   *                            match op with
   *                            | Op.Union -> Abs.union s s'
   *                            | Inter -> Abs.inter s s'
   *                            | Sub -> Abs.sub s s'
   *                            | _ -> assert false
   *                          in
   *                          let ret_t = Op.ret_type op in
   *                          let state_v_out =
   *                            State.create state cost ret_t |> Is_fresh.unwrap
   *                          in
   *                          insert_hyper_edge_if_not_exists graph [ a; a' ] op
   *                            state_v_out)))); *)
  let size' = nb_vertex graph in
  Dump.dump_detailed ~suffix:(sprintf "after-fill-%d" cost) graph;

  Fmt.epr "Pruning: size before=%d, after=%d, removed %f%%\n" size size'
    Float.(100.0 - (of_int size' / of_int size * 100.0))

let fill_up_to_cost (graph : Search_state.t) ops cost =
  let rec fill c =
    if c > cost then false
    else
      let changed, () = did_change @@ fun () -> fill_cost graph ops c in
      changed || fill (c + 1)
  in
  fill 1

module Stats = struct
  type t = {
    n_state_nodes : int;
    n_arg_nodes : int;
    n_covered : int;
    n_refuted : int;
    min_width : int;
    max_width : int;
    median_width : int;
    sat : bool;
  }
end

exception Done of [ `Sat | `Unsat ]

let separators graph target =
  Seq.unfold ~init:(G.succ graph target) ~f:(fun sep ->
      if List.is_empty sep then None
      else
        let sep' =
          List.concat_map sep ~f:(G.succ graph)
          |> List.concat_map ~f:(G.succ graph)
        in
        Some (sep, sep'))

let refine_level n graph =
  V.fold graph ~init:(0, 0) ~f:(fun ((num, dem) as acc) ->
      Node.match_
        ~args:(fun _ -> acc)
        ~state:(fun v ->
          let state = State.state v in
          match state with
          | Bool_vector v -> (num + Abs.Bool_vector.width v, dem + n)
          | _ -> acc))

let refine search_state output refinement =
  let graph = search_state in
  let size = nb_vertex search_state in

  List.iteri refinement ~f:(fun i (r : Refine.Refinement.t) ->
      let context_v = Node.of_args r.context in
      let preds = G.pred graph context_v in
      let cost = List.hd_exn preds |> Node.to_state_exn |> State.cost
      and type_ = Args.output_type r.context in

      List.iter preds ~f:(fun p -> G.remove_edge graph p context_v);

      List.iter r.splits ~f:(fun state ->
          let (Fresh v' | Stale v') = State.create state cost type_ in
          G.add_edge_e graph (Node.of_state v', -1, Node.of_args r.context));

      Dump.dump_detailed ~output ~suffix:(sprintf "fixup-%d" i) graph);

  Dump.dump_detailed ~output ~suffix:"before-fixup" graph;
  fix_up search_state;
  Dump.dump_detailed ~output ~suffix:"after-fixup" graph;

  let size' = nb_vertex search_state in
  Fmt.epr "Pruning: size before=%d, after=%d, removed %f%%\n" size size'
    Float.(100.0 - (of_int size' / of_int size * 100.0))

let rec extract_program graph selected_edges target =
  let args =
    G.succ_e graph target
    |> List.filter ~f:(Set.mem selected_edges)
    |> List.map ~f:(fun (_, _, v) -> v)
    |> List.map ~f:Node.to_args_exn
  in
  match args with
  | [ a ] ->
      `Apply
        ( Args.op a,
          G.succ graph (Node.of_args a)
          |> List.map ~f:(extract_program graph selected_edges) )
  | args ->
      Error.create "Too many args" args [%sexp_of: Args.t list] |> Error.raise

let refute search_state output =
  let graph = search_state in
  match
    V.find_map graph ~f:(fun v ->
        match Node.to_state v with
        | Some v when Abs.contains (State.state v) output -> Some v
        | _ -> None)
  with
  | Some target ->
      let seps = separators graph (Node.of_state target) |> Seq.to_list in
      let seps, last_sep =
        match List.rev seps with
        | last :: rest -> (List.rev rest, last)
        | _ -> failwith "No separators"
      in

      Dump.dump_detailed ~output ~suffix:"before-refinement" ~depth:0 graph;

      let refinement =
        List.find_map seps ~f:(fun sep ->
            let open Option.Let_syntax in
            let%map r =
              Refine.get_refinement search_state target output sep
              |> Either.First.to_option
            in
            (sep, r))
      in
      ( match refinement with
      | Some (_, r) -> refine search_state output r
      | None -> (
          match Refine.get_refinement search_state target output last_sep with
          | First r -> refine search_state output r
          | Second selected_edges ->
              Fmt.epr "Could not refute: %a" Sexp.pp_hum
                ( [%sexp_of: Program.t]
                @@ extract_program graph selected_edges (Node.of_state target)
                );
              raise (Done `Sat) ) );
      true
  | None -> false

let count_compressible graph =
  let args =
    V.filter_map graph ~f:Node.to_args
    |> List.map ~f:(fun v ->
           let v = Node.of_args v in
           (G.pred graph v, G.succ graph v))
  in
  let module Key = struct
    module T = struct
      type t = Node.t list * Node.t list [@@deriving compare, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end in
  let set_args = Set.of_list (module Key) args in
  Fmt.epr "Compressed args: reduces %d to %d\n" (List.length args)
    (Set.length set_args)

let synth () =
  let bench = Set_once.get_exn Global.bench [%here] in
  let search_state = create () in
  let graph = search_state in

  (* Add inputs to the state space graph. *)
  List.iter bench.ops ~f:(fun op ->
      match Op.type_ op with
      | [], ret_t ->
          let state = Abs.top ret_t in
          let state_v_out = State.create state 1 ret_t |> Is_fresh.unwrap in
          insert_hyper_edge_if_not_exists graph [] op state_v_out
      | _ -> ());

  let status =
    try
      for cost = 1 to !Global.max_cost do
        ( until_done @@ fun () ->
          let changed =
            until_done @@ fun () ->
            refute search_state @@ Conc.bool_vector bench.output
          in
          let changed' = fill_up_to_cost search_state bench.ops cost in
          Fmt.epr "Changed: %b Cost: %d\n%!" (changed || changed') cost;
          changed || changed' )
        |> ignore
      done;
      `Unsat
    with Done status -> status
  in

  let stats =
    Stats.
      {
        n_state_nodes = V.filter graph ~f:Node.is_state |> List.length;
        n_arg_nodes = V.filter graph ~f:Node.is_args |> List.length;
        n_covered = -1;
        n_refuted = -1;
        min_width = -1;
        max_width = -1;
        median_width = -1;
        sat = (match status with `Sat -> true | `Unsat -> false);
      }
  in
  (search_state, stats)
