open Ast
open Search_state
open Params
module C = Cone.Make (Search_state.G)
open C

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

exception Found_target of State.t

let create_hyper_edge ss cost op args =
  match insert_hyper_edge_if_not_exists ss args op cost with
  | Some state_v_out ->
      let output = Conc.bool_vector (params ss).bench.output in
      if Abs.contains (State.state ss state_v_out) output then
        raise (Found_target state_v_out)
  | None -> ()

let fold_range ~init ~f lo hi =
  let rec fold_range acc i =
    if i >= hi then acc
    else
      let acc' = f acc i in
      fold_range acc' (i + 1)
  in
  fold_range init lo

let roots ss =
  let is_subset v ~of_:v' =
    State.cost ss v = State.cost ss v'
    && Abs.is_subset (State.state ss v) ~of_:(State.state ss v')
  in
  G.Fold.V.filter_map (graph ss)
    ~f:(Node.match_ ~state:Option.return ~args:(fun _ -> None))
  |> List.fold_left ~init:[] ~f:(fun roots v ->
         match List.find roots ~f:(fun v' -> is_subset v ~of_:v') with
         | Some _ -> roots
         | None ->
             v :: List.filter roots ~f:(fun v' -> not (is_subset v' ~of_:v)))

let root_states ss = roots ss |> List.map ~f:(State.state ss)

module State_set = struct
  type t = type_:Type.t -> cost:int -> State.t list
end

let state_set_full ss =
  let cost_tbl = Hashtbl.create (module Int) in
  fun ~type_ ~cost ->
    let type_tbl =
      match Hashtbl.find cost_tbl cost with
      | Some type_tbl -> type_tbl
      | None ->
          let states = states_of_cost ss cost in
          let type_tbl = Hashtbl.create (module Type) in
          List.iter states ~f:(fun s ->
              Hashtbl.add_multi type_tbl ~key:(State.type_ ss s) ~data:s);
          type_tbl
    in
    Hashtbl.find type_tbl type_ |> Option.value ~default:[]

let state_set_roots ss : State_set.t =
  let module Key = struct
    type t = int * Type.t [@@deriving compare, hash, sexp]
  end in
  let tbl = Hashtbl.create (module Key) in
  let root_states = roots ss in
  List.iter root_states ~f:(fun s ->
      Hashtbl.add_multi tbl ~key:(State.cost ss s, State.type_ ss s) ~data:s);
  fun ~type_ ~cost -> Hashtbl.find tbl (cost, type_) |> Option.value ~default:[]

let fill_cost ss (state_set : State_set.t) ops cost =
  let size = nb_vertex ss in
  ( if cost = 1 then
    List.filter ops ~f:(fun op ->
        match op with Op.Cuboid _ | Cylinder _ -> true | _ -> false)
    |> List.iter ~f:(fun op ->
           let arity = Op.arity op in
           let arg_types = Op.args_type op |> Array.of_list in
           let add_hyper_edges =
             fold_range
               ~init:(fun args -> create_hyper_edge ss cost op args)
               ~f:(fun f i args ->
                 state_set ~cost:1 ~type_:arg_types.(i)
                 |> List.iter ~f:(fun v -> f (v :: args)))
               0 arity
           in
           add_hyper_edges [])
  else if cost > 1 then
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
                       state_set
                         ~cost:(Combinat.Int_array.get arg_costs i)
                         ~type_:arg_types.(i)
                       |> List.iter ~f:(fun v -> f (v :: args)))
                     0 arity
                 in
                 add_hyper_edges [])) );

  let size' = nb_vertex ss in

  Fmt.pr "Filling: size before=%d, after=%d, removed %f%%\n%!" size size'
    Float.(100.0 - (of_int size' / of_int size * 100.0))

let[@landmark "fill"] fill_up_to_cost ss ops cost =
  let rec fill c =
    if c > cost then false
    else
      let state_set = state_set_roots ss in
      let changed, () = did_change @@ fun () -> fill_cost ss state_set ops c in
      changed || fill (c + 1)
  in
  fill 1

module Stats = struct
  type t = {
    n_state_nodes : int;
    n_distinct_state_nodes : int;
    n_arg_nodes : int;
    n_roots : int;
    n_refuted : int;
    width : int * int * int;
    arg_in_degree : int * int * int;
    refinement_level : int * int;
    refined : int Hashtbl.M(Args).t;
    ops : Offset.t Op.t Hashtbl.M(Args).t;
    overlap : (float * float * float) Hashtbl.M(Args).t;
    sat : bool;
  }
  [@@deriving sexp_of]

  let pp fmt stats = Sexp.pp_hum fmt @@ [%sexp_of: t] stats

  let global =
    ref
      {
        n_state_nodes = -1;
        n_distinct_state_nodes = -1;
        n_arg_nodes = -1;
        n_roots = -1;
        n_refuted = -1;
        width = (-1, -1, -1);
        arg_in_degree = (-1, -1, -1);
        refinement_level = (-1, -1);
        ops = Hashtbl.create (module Args);
        refined = Hashtbl.create (module Args);
        overlap = Hashtbl.create (module Args);
        sat = false;
      }
end

exception Done of [ `Sat of Program.t | `Unsat ]

let refine_level ss =
  let n = (params ss).n_bits in
  G.Fold.V.fold (graph ss) ~init:(0, 0) ~f:(fun ((num, dem) as acc) ->
      Node.match_
        ~args:(fun _ -> acc)
        ~state:(fun v ->
          let state = State.state ss v in
          match state with
          | Bool_vector v -> (num + Abs.Bool_vector.width v, dem + n)
          | _ -> acc))

let with_size ss f =
  let size = G.nb_vertex (graph ss) in
  let vsize = G.Fold.V.filter (graph ss) ~f:Node.is_state |> List.length in
  let ret = f ss in
  let size' = G.nb_vertex (graph ss) in
  let vsize' = G.Fold.V.filter (graph ss) ~f:Node.is_state |> List.length in

  (* Fmt.pr "Pruning: size before=%d, after=%d, removed %f%%\n%!" size size'
   *   Float.(100.0 - (of_int size' / of_int size * 100.0)); *)
  Fmt.pr "Pruning: size before=%d, after=%d, removed %f%%\n%!" vsize vsize'
    Float.(100.0 - (of_int vsize' / of_int vsize * 100.0));

  ret

let stats_of_list ~compare l =
  let min = Option.value_exn (List.min_elt l ~compare)
  and max = Option.value_exn (List.max_elt l ~compare)
  and median = List.nth_exn (List.sort l ~compare) (List.length l / 2) in
  (min, max, median)

let refine ss (refinement : Refine.Refinement.t) =
  let new_states =
    Map.to_alist refinement
    |> List.concat_map ~f:(fun (arg_v, { old; new_ }) ->
           Hashtbl.incr !Stats.global.refined arg_v;
           Hashtbl.set !Stats.global.ops ~key:arg_v ~data:(Args.op ss arg_v);

           let cost, type_ =
             let old_state =
               G.pred (graph ss) @@ Node.of_args arg_v
               |> List.hd_exn |> Node.to_state_exn
             in
             (State.cost ss old_state, State.type_ ss old_state)
           in

           (* Remove edges to old states *)
           G.pred_e (graph ss) @@ Node.of_args arg_v
           |> List.filter ~f:(fun (state_v, _, _) ->
                  Set.mem old (Node.to_state_exn state_v |> State.state ss))
           |> List.iter ~f:(G.remove_edge_e @@ graph ss);

           (* Insert new states and add edges to args nodes. *)
           let new_states =
             Set.to_list new_
             |> List.map ~f:(fun state ->
                    let (Fresh v' | Stale v') =
                      State.create ss state cost type_
                    in
                    G.add_edge_e (graph ss)
                      (Node.of_state v', -1, Node.of_args arg_v);
                    v')
           in

           let states =
             G.pred (graph ss) @@ Node.of_args arg_v
             |> List.map ~f:(fun v -> State.state ss @@ Node.to_state_exn v)
           in
           print_s [%message (arg_v : Args.t) (states : Abs.t list)];

           let overlaps =
             let states' =
               List.filter_map states ~f:(function
                 | Bool_vector v -> Some v
                 | _ -> None)
             in
             List.concat_map states' ~f:(fun v ->
                 List.map states' ~f:(fun v' ->
                     Abs.Bool_vector.log_overlap (params ss) v v'))
             |> stats_of_list ~compare:[%compare: float]
           in
           Hashtbl.set !Stats.global.overlap ~key:arg_v ~data:overlaps;

           new_states)
  in

  fix_up ss;

  let refined_graph = cone (graph ss) @@ List.map ~f:Node.of_state new_states in

  dump_detailed_graph ~suffix:"refined-graph"
    (* ~separator:(List.mem separator ~equal:[%equal: Node.t]) *)
    ss refined_graph

let refine graph refinement = with_size graph @@ fun g -> refine g refinement

let refute ss target =
  Stats.global := { !Stats.global with n_refuted = !Stats.global.n_refuted + 1 };
  match Refine.get_refinement ss target with
  | First r ->
      refine ss r;
      let roots = roots ss in
      let n_roots = List.length roots in
      Stats.global := { !Stats.global with n_roots };
      validate ss
  | Second p ->
      Fmt.pr "Could not refute: %a" Sexp.pp_hum ([%sexp_of: Program.t] p);
      raise @@ Done (`Sat p)

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
  Fmt.pr "Compressed args: reduces %d to %d\n" (List.length args)
    (Set.length set_args)

let width_stats ss =
  G.Fold.V.filter_map (graph ss)
    ~f:
      (Node.match_
         ~state:(fun s ->
           match State.state ss s with Bool_vector v -> Some v | _ -> None)
         ~args:(fun _ -> None))
  |> List.map ~f:Abs.Bool_vector.width
  |> stats_of_list ~compare:[%compare: int]

let arg_in_degree_stats ss =
  G.Fold.V.filter (graph ss) ~f:Node.is_args
  |> List.map ~f:(G.in_degree (graph ss))
  |> stats_of_list ~compare:[%compare: int]

let synth params =
  let ss = Search_state.create params in

  ( at_exit @@ fun () ->
    Fmt.pr "%a%!" Stats.pp
      {
        !Stats.global with
        n_state_nodes =
          G.Fold.V.filter (graph ss) ~f:Node.is_state |> List.length;
        n_distinct_state_nodes =
          G.Fold.V.filter_map (graph ss) ~f:(fun v ->
              if Node.is_state v then
                Some (Node.to_state_exn v |> State.state ss)
              else None)
          |> List.dedup_and_sort ~compare:[%compare: Abs.t]
          |> List.length;
        n_arg_nodes = G.Fold.V.filter (graph ss) ~f:Node.is_args |> List.length;
        refinement_level = refine_level ss;
        width = width_stats ss;
        arg_in_degree = arg_in_degree_stats ss;
      } );

  (* Add inputs to the state space graph. *)
  List.iter params.bench.ops ~f:(fun op ->
      match Op.type_ op with
      | [], ret_t ->
          let state = Abs.top params ret_t in
          (insert_hyper_edge_if_not_exists ss [] op 1 ~state : _ option)
          |> ignore
      | _ -> ());

  let status =
    try
      for cost = 1 to params.max_cost do
        let _changed =
          until_done @@ fun () ->
          try
            let did_change = fill_up_to_cost ss params.bench.ops cost in
            validate ss;
            did_change
          with Found_target state_v ->
            refute ss state_v;
            true
        in
        Fmt.pr "Finished cost %d\n%!" cost
      done;
      `Unsat
    with Done status -> status
  in

  (match status with `Sat p -> Program.check params p | _ -> ());

  ss

let cegis params =
  let rec extract_program ss state_v =
    match
      G.succ (graph ss) @@ Node.of_state state_v |> List.map ~f:Node.to_args_exn
    with
    | args_v :: _ ->
        `Apply
          ( Args.op ss args_v,
            inputs ss args_v |> List.map ~f:(extract_program ss) )
    | _ -> raise_s [%message "expected at least one argument"]
  in

  let find_program params =
    let ss = Search_state.create params in

    (* Add inputs to the state space graph. *)
    List.iter params.bench.ops ~f:(fun op ->
        match Op.type_ op with
        | [], _ ->
            let state = Abs.lift @@ Conc.eval params op [] in
            (insert_hyper_edge_if_not_exists ss [] op 1 ~state : _ option)
            |> ignore
        | _ -> ());

    try
      for cost = 1 to params.max_cost do
        print_s [%message "filling" (cost : int)];
        (until_done @@ fun () -> fill_up_to_cost ss params.bench.ops cost : bool)
        |> ignore;
        Fmt.pr "Finished cost %d\n%!" cost
      done;
      None
    with Found_target state_v -> Some (extract_program ss state_v)
  in

  let rec eval params (`Apply (op, args)) =
    Conc.eval params op @@ List.map ~f:(eval params) args
  in

  let find_counter input output program =
    let out =
      match eval params program with
      | Bool_vector v -> v
      | _ -> failwith "expected a vector"
    in
    let counter_idx =
      Array.find_mapi out ~f:(fun i v ->
          if Bool.(v <> params.bench.output.(i)) then Some i else None)
    in
    match counter_idx with
    | Some i ->
        let input = params.bench.input.(i) :: input
        and output = params.bench.output.(i) :: output in
        Some (input, output)
    | None -> None
  in

  let rec loop input output =
    print_s [%message "searching" (List.length input : int)];
    let params' =
      {
        params with
        bench =
          {
            params.bench with
            input = Array.of_list input;
            output = Array.of_list output;
          };
      }
    in
    match find_program params' with
    | Some p -> (
        match find_counter input output p with
        | Some (i', o') -> loop i' o'
        | None -> print_s [%message "found solution" (p : Program.t)] )
    | None -> print_s [%message "no solution"]
  in

  loop [] []
