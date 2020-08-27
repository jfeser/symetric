[@@@landmark "auto"]

open Search_state

module Seq = struct
  include Sequence

  let of_array a = init (Array.length a) ~f:(fun i -> a.(i))
end

let value_exn x = Option.value_exn x

module Program = struct
  module T = struct
    type t = [ `Apply of Op.t * t list ] [@@deriving compare, hash, sexp]
  end

  let rec ceval (`Apply (op, args)) =
    match (op, args) with
    | Op.Input x, _ -> x
    | Union, [ x; y ] -> Array.map2_exn (ceval x) (ceval y) ~f:( || )
    | Inter, [ x; y ] -> Array.map2_exn (ceval x) (ceval y) ~f:( && )
    | Sub, [ x; y ] ->
        Array.map2_exn (ceval x) (ceval y) ~f:(fun a b -> a && not b)
    | _ -> assert false

  let rec size (`Apply (op, args)) = 1 + List.sum (module Int) args ~f:size

  include T
  include Comparator.Make (T)
end

module Args_node = struct
  include Args_node0

  let create ~op graph args =
    match
      (Hashtbl.find graph.Search_state.args_table (op, args)
       [@landmark "create.lookup"])
    with
    | Some v -> None
    | None ->
        let args_n = Args_node0.create op in
        let args_v = Node.of_args args_n in
        List.iteri args ~f:(fun i v ->
            add_edge_e graph (args_v, i, Node.of_state v))
        [@landmark "create.add_edges"];
        Hashtbl.set graph.args_table (op, args) args_n
        [@landmark "create.insert"];
        Some args_n
end

module State_node = struct
  include State_node0

  let rec choose_program graph node =
    match children graph node with
    | (op, args) :: _ -> `Apply (op, List.map args ~f:(choose_program graph))
    | _ -> failwith "expected arguments"

  let create_consed ~state ~cost:c (g : Search_state.t) =
    match Hashtbl.find g.state_table (state, c) with
    | Some v ->
        assert (cost v = c);
        `Stale v
    | None ->
        let v = create state c in
        set_states_of_cost g c (v :: states_of_cost g c);
        Hashtbl.add_exn g.state_table (state, c) v;
        `Fresh v

  let create_op ~state ~cost ~op g children =
    match Args_node.create ~op g children with
    | Some args_v -> (
        match[@landmarks "create_op.match"] create_consed ~state ~cost g with
        | `Fresh state_v ->
            add_edge_e g
              (Node.of_state state_v, -1, Node.of_args args_v)
            [@landmarks "create_op.add_edge"];
            Some state_v
        | `Stale state_v ->
            add_edge_e g
              (Node.of_state state_v, -1, Node.of_args args_v)
            [@landmarks "create_op.add_edge"];
            None )
    | None -> None
end

let fill_cost (graph : Search_state.t) cost =
  if cost <= 1 then false
  else
    let arg_cost = cost - 1 in
    let module Comp = Combinat.Composition in
    let added = ref false in
    Comp.create ~n:arg_cost ~k:2
    |> Comp.iter ~f:(fun arg_costs ->
           let c = arg_costs.{0} and c' = arg_costs.{1} in
           states_of_cost graph c
           |> List.iter ~f:(fun (a : State_node.t) ->
                  states_of_cost graph c'
                  |> List.iter ~f:(fun (a' : State_node.t) ->
                         List.iter [ Op.Union; Inter; Sub ] ~f:(fun op ->
                             let state =
                               let s = State_node0.state a
                               and s' = State_node0.state a' in
                               match op with
                               | Op.Union -> Abs.union s s'
                               | Inter -> Abs.inter s s'
                               | Sub -> Abs.sub s s'
                               | _ -> assert false
                             in
                             let did_add =
                               match
                                 State_node.create_op ~state ~cost ~op graph
                                   [ a; a' ]
                               with
                               | None -> false
                               | Some _ -> true
                             in
                             added := !added || did_add))));
    !added

let fill_up_to_cost (graph : Search_state.t) cost =
  let rec fill c =
    if c > cost then false else fill_cost graph c || fill (c + 1)
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

module Reachable (G : Graph.Fixpoint.G) =
  Graph.Fixpoint.Make
    (G)
    (struct
      type vertex = G.V.t

      type edge = G.E.t

      type g = G.t

      type data = bool

      let direction = Graph.Fixpoint.Forward

      let equal = Bool.( = )

      let join = ( || )

      let analyze _ x = x
    end)

module Inv_reachable (G : Graph.Fixpoint.G) =
  Graph.Fixpoint.Make
    (G)
    (struct
      type vertex = G.V.t

      type edge = G.E.t

      type g = G.t

      type data = bool

      let direction = Graph.Fixpoint.Backward

      let equal = Bool.( = )

      let join = ( || )

      let analyze _ x = x
    end)

module View (G : sig
  type t

  module V : sig
    type t
  end

  module E : sig
    type t

    val src : t -> V.t

    val dst : t -> V.t
  end

  val pred_e : t -> V.t -> E.t list

  val succ_e : t -> V.t -> E.t list

  val iter_vertex : (V.t -> unit) -> t -> unit

  val iter_edges_e : (E.t -> unit) -> t -> unit

  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a

  val fold_edges_e : (E.t -> 'a -> 'a) -> t -> 'a -> 'a

  val mem_vertex : t -> V.t -> bool

  val mem_edge_e : t -> E.t -> bool
end) =
struct
  include G

  let pred_e g v =
    if G.mem_vertex g v then
      G.pred_e g v |> List.filter ~f:(fun e -> G.mem_vertex g @@ E.src e)
    else failwith "Node not in graph"

  let succ_e g v =
    if G.mem_vertex g v then
      G.succ_e g v |> List.filter ~f:(fun e -> G.mem_vertex g @@ E.dst e)
    else failwith "Node not in graph"

  let iter_edges_e f g = iter_edges_e (fun e -> if G.mem_edge_e g e then f e) g

  let iter_vertex f g = iter_vertex (fun v -> if G.mem_vertex g v then f v) g

  let fold_edges_e f g x =
    fold_edges_e (fun e x -> if G.mem_edge_e g e then f e x else x) g x

  let fold_vertex f g x =
    fold_vertex (fun v x -> if G.mem_vertex g v then f v x else x) g x
end

let separators graph target =
  Seq.unfold ~init:(succ graph target) ~f:(fun sep ->
      if List.is_empty sep then None
      else
        let sep' =
          List.concat_map sep ~f:(succ graph) |> List.concat_map ~f:(succ graph)
        in
        Some (sep, sep'))

let value_exn x = Option.value_exn x

let should_prune graph separator =
  let separator = Set.of_list (module Node) separator in
  let reaches_separator =
    let module R = Inv_reachable (G) in
    R.analyze (Set.mem separator) graph.Search_state.graph
  and separator_reaches =
    let module R = Reachable (G) in
    R.analyze (Set.mem separator) graph.Search_state.graph
  in
  fun v -> reaches_separator v && not (separator_reaches v)

let prune graph refined =
  let should_prune = should_prune graph refined in
  let size = G.nb_vertex graph.Search_state.graph in
  filter graph ~f:(fun v -> not (should_prune v));
  let size' = G.nb_vertex graph.Search_state.graph in
  Fmt.epr "Pruning: size before=%d, after=%d, removed %f%%\n" size size'
    Float.(100.0 - (of_int size' / of_int size * 100.0))

let fix_up_args graph args =
  let to_remove, states =
    List.filter_map args ~f:(fun v ->
        let args_v = Node.to_args_exn v in
        let succ = succ graph v in
        if List.length succ <> Op.arity (Args_node.op args_v) then
          Some (v, pred graph v)
        else None)
    |> List.unzip
  in
  remove_vertexes graph to_remove;
  List.concat states |> List.dedup_and_sort ~compare:[%compare: Node.t]

let fix_up_states graph states =
  let to_remove, args =
    List.filter_map states ~f:(fun v ->
        Node.to_state_exn v |> ignore;
        let succ = succ graph v in
        if List.is_empty succ then Some (v, pred graph v) else None)
    |> List.unzip
  in
  remove_vertexes graph to_remove;
  List.concat args |> List.dedup_and_sort ~compare:[%compare: Node.t]

let refine_level n graph =
  V.fold graph ~init:(0, 0) ~f:(fun ((num, dem) as acc) ->
      Node.match_
        ~args:(fun _ -> acc)
        ~state:(fun v -> (num + Abs.width (State_node.state v), dem + n)))

let refine graph output separator refinement =
  let size = G.nb_vertex graph.Search_state.graph in
  Dump.dump_detailed ~output ~suffix:"before-refinement" graph;

  let states_to_fix =
    List.concat_map refinement ~f:(function `Split (v, cost, refined) ->
        let pred_edges = pred_e graph (Node.of_args v) in
        (* Remove edges to existing state nodes. *)
        List.iter pred_edges ~f:(G.remove_edge_e graph.Search_state.graph);
        (* Insert split state nodes. *)
        List.iter refined ~f:(fun state ->
            let (`Fresh v' | `Stale v') =
              State_node.create_consed ~state ~cost graph
            in
            add_edge_e graph (Node.of_state v', -1, Node.of_args v));

        (* Extract the states where we removed an edge. *)
        List.map pred_edges ~f:(fun (v, _, _) -> v))
  in

  let rec loop states_to_fix =
    let args_to_fix = fix_up_states graph states_to_fix in
    if not (List.is_empty args_to_fix) then
      let states_to_fix = fix_up_args graph args_to_fix in
      loop states_to_fix
  in
  loop states_to_fix;

  let size' = G.nb_vertex graph.Search_state.graph in
  Fmt.epr "Pruning: size before=%d, after=%d, removed %f%%\n" size size'
    Float.(100.0 - (of_int size' / of_int size * 100.0));
  Dump.dump_detailed ~output ~suffix:"after-refinement" graph;

  let num, dem = refine_level (Array.length output) graph in
  Fmt.epr "Refine level: %d/%d (%f%%)\n" num dem
    Float.(of_int num / of_int dem * 100.0)

let rec extract_program graph selected_edges target =
  let args =
    succ_e graph target
    |> List.filter ~f:(Set.mem selected_edges)
    |> List.map ~f:(fun (_, _, v) -> v)
    |> List.map ~f:Node.to_args_exn
  in
  match args with
  | [ a ] ->
      `Apply
        ( Args_node.op a,
          succ graph (Node.of_args a)
          |> List.map ~f:(extract_program graph selected_edges) )
  | args ->
      Error.create "Too many args" args [%sexp_of: Args_node.t list]
      |> Error.raise

let refute graph output =
  match
    V.find_map graph ~f:(fun v ->
        match Node.to_state v with
        | Some v when Abs.contains (State_node.state v) output -> Some v
        | _ -> None)
  with
  | Some target ->
      let seps = separators graph (Node.of_state target) |> Seq.to_list in
      let seps, last_sep =
        match List.rev seps with
        | last :: rest -> (List.rev rest, last)
        | _ -> failwith "No separators"
      in

      let refinement =
        List.find_map seps ~f:(fun sep ->
            let open Option.Let_syntax in
            let%map r =
              Refine.get_refinement graph target output sep
              |> Either.First.to_option
            in
            (sep, r))
      in
      ( match refinement with
      | Some (sep, r) -> refine graph output sep r
      | None -> (
          match Refine.get_refinement graph target output last_sep with
          | First r -> refine graph output last_sep r
          | Second selected_edges ->
              Fmt.epr "Could not refute: %a" Sexp.pp_hum
                ( [%sexp_of: Program.t]
                @@ extract_program graph selected_edges (Node.of_state target)
                );
              raise (Done `Sat) ) );
      true
  | None -> false

let synth ?(no_abstraction = false) inputs output =
  let graph = create !Global.max_cost in

  let rec loop cost =
    if cost > !Global.max_cost then raise (Done `Unsat);
    let rec strengthen_and_cover () =
      let did_strengthen = refute graph output in
      if did_strengthen then strengthen_and_cover ()
    in
    strengthen_and_cover ();

    let changed = fill_up_to_cost graph cost in
    Fmt.epr "Changed: %b Cost: %d\n%!" changed cost;
    let cost = if changed then cost else cost + 1 in
    loop cost
  in

  (* Add inputs to the state space graph. *)
  List.iter inputs ~f:(fun input ->
      let state = if no_abstraction then Abs.lift input else Abs.top in
      State_node.create_op ~state ~cost:1 ~op:(Op.Input input) graph []
      |> ignore);

  try loop 1
  with Done status ->
    let widths =
      V.filter_map graph ~f:(fun v ->
          match Node.to_state v with
          | Some v -> Some (Abs.width @@ State_node.state v)
          | _ -> None)
      |> List.sort ~compare:[%compare: int]
      |> Array.of_list
    in
    ( graph,
      Stats.
        {
          n_state_nodes = V.filter graph ~f:Node.is_state |> List.length;
          n_arg_nodes = V.filter graph ~f:Node.is_args |> List.length;
          n_covered = -1;
          n_refuted = !Global.n_refuted;
          min_width = widths.(0);
          max_width = widths.(Array.length widths - 1);
          median_width = widths.(Array.length widths / 2);
          sat = (match status with `Sat -> true | `Unsat -> false);
        } )

let sample ?(state = Random.State.default) inputs =
  let open Grammar in
  let named_inputs = List.mapi inputs ~f:(fun i x -> (sprintf "i%d" i, x)) in
  let input_rules =
    List.map named_inputs ~f:(fun (n, _) -> Rule.create "p" (Term.app n []) [])
  in
  let g =
    input_rules
    @ [
        Rule.create "p"
          (Term.app "and" [ Term.nonterm "p"; Term.nonterm "p" ])
          [];
        Rule.create "p"
          (Term.app "or" [ Term.nonterm "p"; Term.nonterm "p" ])
          [];
        Rule.create "p"
          (Term.app "diff" [ Term.nonterm "p"; Term.nonterm "p" ])
          [];
      ]
  in
  let rec to_prog = function
    | App (func, args) ->
        let op =
          match func with
          | "and" -> Op.Inter
          | "or" -> Union
          | "diff" -> Sub
          | _ -> (
              match
                List.Assoc.find ~equal:[%compare.equal: string] named_inputs
                  func
              with
              | Some i -> Input i
              | None -> failwith "unexpected function" )
        in
        let args = List.map args ~f:to_prog in
        `Apply (op, args)
    | _ -> failwith "unexpected term"
  in
  let rec sample_prog () =
    let p = to_prog (Grammar.sample ~state "p" g :> Untyped_term.t) in
    if Program.size p > !Global.max_cost then sample_prog () else p
  in
  sample_prog ()

let check_search_space ?(n = 100_000) inputs graph =
  let rec loop i =
    if i > n then (
      Fmt.epr "Checked %d programs and found no counterexamples\n" n;
      Ok () )
    else
      let prog = sample inputs in
      let cstate = Program.ceval prog in
      match
        V.find_map graph ~f:(fun v ->
            match Node.to_state v with
            | Some v when Abs.contains (State_node.state v) cstate -> Some v
            | _ -> None)
      with
      | Some v -> loop (i + 1)
      | None ->
          Fmt.epr "Missed program %a with size %d and state %a\n" Sexp.pp
            ([%sexp_of: Program.t] prog)
            (Program.size prog) Conc.pp cstate;
          Error cstate
  in
  loop 0

let random_likely_unsat ?(state = Random.State.default) n k =
  let inputs =
    List.init n ~f:(fun _ -> Array.init k ~f:(fun _ -> Random.State.bool state))
  in
  let output = Array.init k ~f:(fun _ -> Random.State.bool state) in
  (inputs, output)

let random_sat ?(state = Random.State.default) n k =
  let inputs =
    List.init n ~f:(fun _ -> Array.init k ~f:(fun _ -> Random.State.bool state))
  in
  let output = sample ~state inputs |> Program.ceval in
  (inputs, output)

let random_io ?(state = Random.State.default) ~n ~k =
  (* if Random.State.bool state then random_sat ~state n k
   * else *)
  random_likely_unsat ~state n k
