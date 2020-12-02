[@@@landmark "auto"]

open Ast
open Search_state
open Params

open Cone.Make (Search_state.G)

module Seq = struct
  include Sequence

  let of_array a = init (Array.length a) ~f:(fun i -> a.(i))
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

let iter_arg ss cost type_ ~f =
  states_of_cost ss cost
  |> List.iter ~f:(fun v ->
         if [%compare.equal: Type.t] (State.type_ ss v) type_ then f v)

let create_hyper_edge ss cost op args =
  let arg_states = List.map args ~f:(State.state ss) in
  let out_state = Abs.eval (params ss) op arg_states in
  let out_type = Op.ret_type op in
  let state_v_out =
    State.create ss out_state cost out_type |> Is_fresh.unwrap
  in
  insert_hyper_edge_if_not_exists ss args op state_v_out

let fold_range ~init ~f lo hi =
  let rec fold_range acc i =
    if i >= hi then acc
    else
      let acc' = f acc i in
      fold_range acc' (i + 1)
  in
  fold_range init lo

let fill_cost ss ops cost =
  let size = nb_vertex ss in
  ( if cost > 1 then
    let arg_cost = cost - 1 in

    List.iter ops ~f:(fun op ->
        let arity = Op.arity op in
        let arg_types = Op.args_type op |> Array.of_list in
        let module Comp = Combinat.Composition in
        if arity > 0 then
          Comp.create ~n:arg_cost ~k:arity
          |> Comp.iter ~f:(fun arg_costs ->
                 let add_hyper_edges =
                   fold_range
                     ~init:(fun args -> create_hyper_edge ss cost op args)
                     ~f:(fun f i args ->
                       let type_ = arg_types.(i) in
                       let states =
                         states_of_cost ss (Combinat.Int_array.get arg_costs i)
                       in
                       List.iter states ~f:(fun v ->
                           if [%compare.equal: Type.t] (State.type_ ss v) type_
                           then f (v :: args)))
                     0 arity
                 in
                 add_hyper_edges [])) );

  let size' = nb_vertex ss in

  Fmt.epr "Pruning: size before=%d, after=%d, removed %f%%\n" size size'
    Float.(100.0 - (of_int size' / of_int size * 100.0))

let fill_up_to_cost ss ops cost =
  let rec fill c =
    if c > cost then false
    else
      let changed, () = did_change @@ fun () -> fill_cost ss ops c in
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

exception Done of [ `Sat of Program.t | `Unsat ]

let refine_level n ss =
  G.Fold.V.fold (graph ss) ~init:(0, 0) ~f:(fun ((num, dem) as acc) ->
      Node.match_
        ~args:(fun _ -> acc)
        ~state:(fun v ->
          let state = State.state ss v in
          match state with
          | Bool_vector v -> (num + Abs.Bool_vector.width v, dem + n)
          | _ -> acc))

let with_size graph f =
  let size = nb_vertex graph in
  let ret = f graph in
  let size' = nb_vertex graph in
  Fmt.epr "Pruning: size before=%d, after=%d, removed %f%%\n" size size'
    Float.(100.0 - (of_int size' / of_int size * 100.0));
  ret

let refine ss refinement =
  let new_states =
    List.concat_map refinement ~f:(fun (r : Refine.Refinement.t) ->
        let cost, type_ =
          let old_state =
            G.pred (graph ss) @@ Node.of_args r.old
            |> List.hd_exn |> Node.to_state_exn
          in
          (State.cost ss old_state, State.type_ ss old_state)
        in

        (* Remove edges to old states *)
        G.pred_e (graph ss) @@ Node.of_args r.old
        |> List.iter ~f:(G.remove_edge_e @@ graph ss);

        (* Insert new states and add edges to args nodes. *)
        let new_states =
          Set.to_list r.new_
          |> List.map ~f:(fun state ->
                 let (Fresh v' | Stale v') = State.create ss state cost type_ in
                 G.add_edge_e (graph ss)
                   (Node.of_state v', -1, Node.of_args r.old);
                 v')
        in

        new_states)
  in

  fix_up ss;

  let refined_graph = cone (graph ss) @@ List.map ~f:Node.of_state new_states in
  dump_detailed_graph ~suffix:"refined-graph"
    (* ~separator:(List.mem separator ~equal:[%equal: Node.t]) *)
    ss refined_graph

let refine graph refinement = with_size graph @@ fun g -> refine g refinement

let refute ss output =
  match
    G.Fold.V.find_map (graph ss) ~f:(fun v ->
        match Node.to_state v with
        | Some v when Abs.contains (State.state ss v) output -> Some v
        | _ -> None)
  with
  | Some target ->
      ( match Refine.get_refinement ss target with
      | First r -> refine ss r
      | Second p ->
          Fmt.epr "Could not refute: %a" Sexp.pp_hum ([%sexp_of: Program.t] p);
          raise @@ Done (`Sat p) );
      true
  | None -> false

let count_compressible graph =
  let args =
    G.Fold.V.filter_map graph ~f:Node.to_args
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

let synth params =
  let ss = Search_state.create params in

  (* Add inputs to the state space graph. *)
  List.iter params.bench.ops ~f:(fun op ->
      match Op.type_ op with
      | [], ret_t ->
          let state = Abs.top ret_t in
          let state_v_out = State.create ss state 1 ret_t |> Is_fresh.unwrap in
          insert_hyper_edge_if_not_exists ss [] op state_v_out
      | _ -> ());

  let output = Conc.bool_vector params.bench.output in
  let status =
    try
      for cost = 1 to params.max_cost do
        ( until_done @@ fun () ->
          let changed = until_done @@ fun () -> refute ss output in
          let changed' = fill_up_to_cost ss params.bench.ops cost in
          Fmt.epr "Changed: %b Cost: %d\n%!" (changed || changed') cost;
          changed || changed' )
        |> ignore
      done;
      `Unsat
    with Done status -> status
  in

  (match status with `Sat p -> Program.check params p | _ -> ());

  let stats =
    Stats.
      {
        n_state_nodes =
          G.Fold.V.filter (graph ss) ~f:Node.is_state |> List.length;
        n_arg_nodes = G.Fold.V.filter (graph ss) ~f:Node.is_args |> List.length;
        n_covered = -1;
        n_refuted = -1;
        min_width = -1;
        max_width = -1;
        median_width = -1;
        sat = (match status with `Sat _ -> true | `Unsat -> false);
      }
  in
  (ss, stats)
