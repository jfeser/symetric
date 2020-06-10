open! Core

let enable_dump = ref false

module State = struct
  module T = struct
    type t = bool array [@@deriving compare, sexp]

    let hash x = [%hash: bool list] (Array.to_list x)

    let hash_fold_t s x = [%hash_fold: bool list] s (Array.to_list x)
  end

  include T

  module O : Comparable.Infix with type t := t = struct
    include T
    include Comparable.Make (T)
  end

  let pp = Fmt.(array bool)
end

module Abs_state = struct
  type t = bool Map.M(Int).t [@@deriving compare, hash, sexp]

  let top = Map.empty (module Int)

  let pp =
    Fmt.using (fun m ->
        Map.to_alist m |> List.map ~f:(fun (k, v) -> (Bool.to_int v, k)))
    @@ Fmt.list ~sep:(Fmt.any " ")
    @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "_%d")

  let is_subset_a s ~of_:s' =
    Map.for_alli s ~f:(fun ~key ~data:x ->
        match Map.find s' key with Some x' -> Bool.(x = x') | None -> false)

  let is_superset_c s ~of_:s' =
    Map.for_alli s ~f:(fun ~key:i ~data:v -> Bool.(s'.(i) = v))

  let lift s =
    Array.mapi s ~f:(fun i x -> (i, x))
    |> Array.to_list
    |> Map.of_alist_exn (module Int)
end

module Op = struct
  type t = [ `Input of State.t | `Union | `Inter | `Sub ]
  [@@deriving compare, hash, sexp]

  let pp fmt op =
    let str =
      match op with
      | `Input _ -> "in"
      | `Union -> "or"
      | `Inter -> "and"
      | `Sub -> "diff"
    in
    Fmt.pf fmt "%s" str
end

module Node0 = struct
  let time = ref 0

  type t = {
    id : int;
    op : Op.t;
    cost : int;
    cstate : State.t;
    mutable state : Abs_state.t;
    mutable covered : bool;
    mutable last_mod_time : int;
  }

  let set_state n s = n.state <- s

  let cover n =
    incr time;
    n.covered <- true;
    n.last_mod_time <- !time

  let uncover n =
    incr time;
    n.covered <- false;
    n.last_mod_time <- !time

  let pp fmt ({ state; op; _ } : t) =
    Fmt.pf fmt "%a %a" Op.pp op Abs_state.pp state

  let hash n = [%hash: State.t] n.cstate

  let hash_fold_t state n = [%hash_fold: State.t] state n.cstate

  let equal n n' = [%compare.equal: State.t] n.cstate n'.cstate

  let compare n n' = [%compare: State.t] n.cstate n'.cstate
end

let union =
  Map.merge ~f:(fun ~key:_ ->
    function
    | `Both (x, x') -> Some (x || x')
    | `Left true | `Right true -> Some true
    | `Left false | `Right false -> None)

let inter =
  Map.merge ~f:(fun ~key:_ ->
    function
    | `Both (x, x') -> Some (x && x')
    | `Left false | `Right false -> Some false
    | `Left true | `Right true -> None)

let sub =
  Map.merge ~f:(fun ~key:_ ->
    function
    | `Both (x, x') -> Some (x && not x')
    | `Left false | `Right true -> Some false
    | `Left true | `Right false -> None)

let contains a c = Map.for_alli a ~f:(fun ~key:i ~data:v -> Bool.(c.(i) = v))

module E = struct
  type t = int [@@deriving compare]

  let compare = [%compare: t]

  let default = -1
end

module G = struct
  module G = struct
    include Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node0) (E)

    let graph_attributes _ = []

    let default_vertex_attributes _ = []

    let vertex_name n = Fmt.str "%d" n.Node0.id

    let vertex_attributes n = [ `HtmlLabel (Fmt.str "%a" Node0.pp n) ]

    let get_subgraph _ = None

    let default_edge_attributes _ = []

    let edge_attributes (_, i, _) = [ `Label (sprintf "%d" i) ]
  end

  include G
  include Graph.Graphviz.Dot (G)

  let filter_vertex g ~f =
    fold_vertex (fun x xs -> if f x then x :: xs else xs) g []

  let map_vertex g ~f = fold_vertex (fun x xs -> f x :: xs) g []

  let exists_vertex g ~f = fold_vertex (fun x xs -> f x || xs) g false

  let succ g v =
    succ_e g v
    |> List.sort ~compare:(fun (_, x, _) (_, x', _) -> [%compare: int] x x')
    |> List.map ~f:(fun (_, _, v) -> v)
end

let ceval' op args =
  match (op, args) with
  | `Input x, _ -> x
  | `Union, [ x; y ] -> Array.map2_exn x.Node0.cstate y.cstate ~f:( || )
  | `Inter, [ x; y ] -> Array.map2_exn x.cstate y.cstate ~f:( && )
  | `Sub, [ x; y ] ->
      Array.map2_exn x.cstate y.cstate ~f:(fun a b -> a && not b)
  | _ -> assert false

let rec ceval g v = ceval' v.Node0.op @@ G.succ g v

let eval g v =
  let args = G.succ g v in
  match (v.op, args) with
  | `Input _, _ -> v.Node0.state
  | `Union, [ v'; v'' ] -> union v'.state v''.state
  | `Inter, [ v'; v'' ] -> inter v'.state v''.state
  | `Sub, [ v'; v'' ] -> sub v'.state v''.state
  | _ -> failwith "Unexpected args"

module Program = struct
  module T = struct
    type t = [ `Apply of Op.t * t list ] [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)
end

module Node = struct
  include Node0

  let rec to_program graph node =
    `Apply (node.op, G.succ graph node |> List.map ~f:(to_program graph))

  let create ?(covered = false) ~state ~cost ~op graph children =
    let cstate = ceval' op children in
    if
      G.exists_vertex graph ~f:(fun v ->
          [%compare.equal: State.t] v.cstate cstate)
    then None
    else (
      incr time;
      let node =
        {
          id = G.nb_vertex graph;
          op;
          cost;
          state;
          cstate;
          covered;
          last_mod_time = !time;
        }
      in
      G.add_vertex graph node;
      List.iteri children ~f:(fun i x -> G.add_edge_e graph (node, i, x));
      Some node )

  let create_exn ?covered ~state ~cost ~op graph children =
    Option.value_exn (create ?covered ~state ~cost ~op graph children)
end

let rec fill graph cost =
  if cost <= 1 then false
  else if fill graph (cost - 1) then true
  else
    let arg_cost = cost - 1 in
    let module Part = Combinat.Partition in
    let module Perm = Combinat.Permutation.Of_list in
    let of_cost c =
      G.filter_vertex ~f:(fun v -> (not v.Node.covered) && v.cost = c) graph
    in

    let added = ref false in
    Part.create ~n:arg_cost ~parts:2
    |> Part.iter ~f:(fun arg_costs ->
           let arg_costs =
             List.init (Bigarray.Array1.dim arg_costs) ~f:(fun i ->
                 arg_costs.{i})
           in
           Perm.(create arg_costs |> to_list)
           |> List.dedup_and_sort ~compare:[%compare: int list]
           |> List.iter ~f:(fun arg_costs ->
                  match arg_costs with
                  | [ c; c' ] ->
                      of_cost c
                      |> List.iter ~f:(fun a ->
                             of_cost c'
                             |> List.iter ~f:(fun a' ->
                                    List.iter [ `Union; `Inter; `Sub ]
                                      ~f:(fun op ->
                                        let state =
                                          match op with
                                          | `Union ->
                                              union a.Node.state a'.Node.state
                                          | `Inter -> inter a.state a'.state
                                          | `Sub -> sub a.state a'.state
                                        in
                                        let did_add =
                                          Node.create ~state ~cost
                                            ~op:(op :> Op.t)
                                            graph [ a; a' ]
                                          |> Option.is_some
                                        in
                                        added := !added || did_add)))
                  | _ -> failwith "Unexpected costs"));
    !added

let prune g (nodes : Node.t list) =
  let min_mod_time =
    Option.value_exn
      ( List.map nodes ~f:(fun n -> n.last_mod_time)
      |> List.min_elt ~compare:[%compare: int] )
  in
  G.iter_vertex
    (fun v -> if v.last_mod_time > min_mod_time then Node.uncover v)
    g;

  let rec process = function
    | v :: vs ->
        let work =
          G.pred g v
          |> List.filter_map ~f:(fun v' ->
                 let old = v'.Node.state and new_ = eval g v' in

                 Fmt.epr "Prune old: %a new: %a %a\n" Abs_state.pp old
                   Abs_state.pp new_ Node.pp v';
                 assert (Abs_state.is_subset_a old ~of_:new_);
                 if [%compare.equal: Abs_state.t] old new_ then None
                 else (
                   Node.set_state v' new_;
                   Some v' ))
        in
        process (work @ vs)
    | [] -> ()
  in
  process nodes

let refine graph node bad =
  let conc = ceval graph node and old = node.state in

  (* The bad behavior should not be the same as the concrete behavior, the
     initial state should abstract the bad behavior and the concrete behavior.
  *)
  assert (
    (not ([%compare.equal: State.t] bad conc))
    && contains old bad && contains old conc );

  let len = Array.length conc in
  let rec loop i =
    if i >= len then failwith "Could not refine"
    else if Bool.(conc.(i) <> bad.(i)) then
      Map.add_exn node.state ~key:i ~data:conc.(i)
    else loop (i + 1)
  in
  let new_ = loop 0 in

  (* Check that the new state refines the old one, does not contain the bad
     state, and still abstracts the concrete behavior. *)
  assert (
    Abs_state.is_subset_a old ~of_:new_
    && (not (contains new_ bad))
    && contains new_ conc );

  Node.set_state node new_;
  [ node ]

let rec refine_children graph node =
  let input_nodes = G.succ graph node |> List.to_array in
  if Array.is_empty input_nodes then []
  else
    let conc_inputs = Array.map input_nodes ~f:(ceval graph)
    and old_inputs = Array.map input_nodes ~f:(fun n -> n.Node.state) in

    let n = Array.length conc_inputs and k = Array.length conc_inputs.(0) in

    Array.iteri old_inputs ~f:(fun i old ->
        (* The old state contains the concrete behavior. *)
        assert (contains old conc_inputs.(i)));

    let changed = Array.create n false in
    let rec loop i j =
      if i >= n || j >= k then failwith "Could not refine";

      Node.set_state input_nodes.(i)
        (Map.set input_nodes.(i).state ~key:j ~data:conc_inputs.(i).(j));
      changed.(i) <- true;

      if not (Abs_state.is_subset_a node.state ~of_:(eval graph node)) then
        if j = k - 1 then loop (i + 1) 0 else loop i (j + 1)
    in
    loop 0 0;

    (* The new input states produce an output that refines the abstract output.
       *)
    assert (Abs_state.is_subset_a node.state ~of_:(eval graph node));

    Array.iteri input_nodes ~f:(fun i node ->
        let old = old_inputs.(i)
        and conc = conc_inputs.(i)
        and new_ = node.state in
        (* The new state refines the old state. *)
        assert (Abs_state.is_subset_a old ~of_:new_);

        (* The new state contains the concrete behavior. *)
        assert (contains new_ conc));

    Array.filter_mapi changed ~f:(fun i c ->
        if c then Some input_nodes.(i) else None)
    |> Array.to_list
    |> List.concat_map ~f:(refine_children graph)

let rec strengthen graph node bad_out =
  assert (contains node.Node.state bad_out);
  let to_prune = refine graph node bad_out in
  let to_prune' = refine_children graph node in
  assert (not (contains node.state bad_out));
  to_prune @ to_prune'

let test_graph () =
  let g = G.create () in
  let v1 =
    Node.create_exn
      ~state:(Map.singleton (module Int) 0 true)
      ~op:(`Input [| true; true |])
      ~cost:1 g []
  and v2 =
    Node.create_exn
      ~state:(Map.singleton (module Int) 1 true)
      ~op:(`Input [| true; false |])
      ~cost:1 g []
  in
  let v3 =
    Node.create_exn
      ~state:(Map.empty (module Int))
      ~op:`Inter ~cost:1 g [ v1; v2 ]
  in
  (g, v1, v2, v3)

let%expect_test "" =
  let g, v1, v2, v3 = test_graph () in
  refine g v3 [| false; false |] |> ignore;
  Out_channel.flush stdout;
  G.output_graph stdout g;
  [%expect
    {|
    digraph G {
      0 [label=<in 1_0>, ];
      1 [label=<in 1_1>, ];
      2 [label=<and 1_0>, ];


      2 -> 0 [label="0", ];
      2 -> 1 [label="1", ];

      } |}]

let%expect_test "" =
  let g, v1, v2, v3 = test_graph () in
  strengthen g v3 [| false; false |];
  Out_channel.flush stdout;
  G.output_graph stdout g;
  [%expect
    {|
    digraph G {
      0 [label=<in 1_0>, ];
      1 [label=<in 1_0 1_1>, ];
      2 [label=<and 1_0>, ];


      2 -> 0 [label="0", ];
      2 -> 1 [label="1", ];

      } |}]

let%expect_test "" =
  let g = G.create () in
  Node.create_exn
    ~state:(Map.singleton (module Int) 0 true)
    ~op:(`Input [| true; true |])
    ~cost:1 g []
  |> ignore;
  Node.create_exn
    ~state:(Map.singleton (module Int) 1 true)
    ~op:(`Input [| true; false |])
    ~cost:1 g []
  |> ignore;
  fill g 3 |> ignore;
  G.output_graph stdout g;
  [%expect
    {|
    digraph G {
      0 [label=<in 1_0>, ];
      1 [label=<in 1_1>, ];
      2 [label=<or 1_1>, ];
      3 [label=<and 1_1>, ];
      4 [label=<diff 0_1>, ];
      5 [label=<or 1_0 1_1>, ];
      6 [label=<and >, ];
      7 [label=<diff 0_0>, ];
      8 [label=<or 1_0 1_1>, ];
      9 [label=<and >, ];
      10 [label=<diff 0_1>, ];
      11 [label=<or 1_0>, ];
      12 [label=<and 1_0>, ];
      13 [label=<diff 0_0>, ];


      2 -> 1 [label="0", ];
      2 -> 1 [label="1", ];
      3 -> 1 [label="0", ];
      3 -> 1 [label="1", ];
      4 -> 1 [label="0", ];
      4 -> 1 [label="1", ];
      5 -> 0 [label="1", ];
      5 -> 1 [label="0", ];
      6 -> 0 [label="1", ];
      6 -> 1 [label="0", ];
      7 -> 0 [label="1", ];
      7 -> 1 [label="0", ];
      8 -> 0 [label="0", ];
      8 -> 1 [label="1", ];
      9 -> 0 [label="0", ];
      9 -> 1 [label="1", ];
      10 -> 0 [label="0", ];
      10 -> 1 [label="1", ];
      11 -> 0 [label="0", ];
      11 -> 0 [label="1", ];
      12 -> 0 [label="0", ];
      12 -> 0 [label="1", ];
      13 -> 0 [label="0", ];
      13 -> 0 [label="1", ];

      } |}]

let%expect_test "" =
  let g, _, _, _ = test_graph () in
  G.filter_vertex ~f:(fun v -> (not v.Node.covered) && v.cost = 1) g
  |> List.iter ~f:(Fmt.epr "%a\n" Node.pp);
  [%expect {|
    and
    in 1_1
    in 1_0 |}]

module Stats = struct
  type t = {
    n_nodes : int;
    n_covered : int;
    n_refuted : int;
    min_width : int;
    max_width : int;
    median_width : int;
    sat : bool;
  }
end

exception Done of [ `Sat | `Unsat ]

let synth ?(max_cost = 20) ?(no_abstraction = false) inputs output =
  let graph = G.create () and step = ref 0 in

  let module Viz = Graph.Graphviz.Dot (struct
    include G

    let graph_attributes _ = []

    let default_vertex_attributes _ = []

    let vertex_name n = Fmt.str "%d" n.Node0.id

    let vertex_attributes n =
      [ `HtmlLabel (Fmt.str "%a" Node0.pp n) ]
      @
      if n.covered then [ `Style `Dotted ]
      else [] @ if contains n.state output then [ `Style `Bold ] else []

    let get_subgraph _ = None

    let default_edge_attributes _ = []

    let edge_attributes (_, i, _) = [ `Label (sprintf "%d" i) ]
  end) in
  let dump () =
    if !enable_dump then (
      if !step >= 100 then exit 1;
      Out_channel.with_file (sprintf "out%d.dot" !step) ~f:(fun ch ->
          Viz.output_graph ch graph);
      Out_channel.flush stdout;
      incr step )
  in

  let n_refuted = ref 0 in

  let rec loop cost =
    if cost > max_cost then raise (Done `Unsat);
    let rec iloop () =
      G.filter_vertex graph ~f:(fun v -> not v.covered)
      |> List.iter ~f:(fun (v : Node.t) ->
             if
               G.exists_vertex graph ~f:(fun (v' : Node.t) ->
                   (not v'.covered) && v'.cost <= v.cost
                   && (not ([%compare.equal: Node.t] v v'))
                   && Abs_state.is_subset_a v'.state ~of_:v.state)
             then (
               Fmt.epr "Covering: %a\n" Node.pp v;
               Node.cover v ));
      dump ();

      let did_strengthen = ref false in
      G.filter_vertex graph ~f:(fun v -> not v.covered)
      |> List.iter ~f:(fun v ->
             if contains v.Node.state output then
               if [%compare.equal: State.t] (ceval graph v) output then
                 raise (Done `Sat)
               else (
                 incr n_refuted;
                 Fmt.epr "Refuting: %a\n" Sexp.pp_hum
                   ([%sexp_of: Program.t] (Node.to_program graph v));
                 let to_prune = strengthen graph v output in
                 dump ();
                 prune graph to_prune;
                 dump ();
                 did_strengthen := true ));
      dump ();
      if !did_strengthen then iloop ()
    in

    iloop ();

    let changed = fill graph cost in
    Fmt.epr "Changed: %b Cost: %d\n" changed cost;
    if changed then dump ();
    let cost = if changed then cost else cost + 1 in
    loop cost
  in

  (* Add inputs to the state space graph. *)
  List.iter inputs ~f:(fun input ->
      let state =
        if no_abstraction then Abs_state.lift input else Map.empty (module Int)
      in
      Node.create ~state ~cost:1 ~op:(`Input input) graph [] |> ignore);
  dump ();

  try loop 1
  with Done status ->
    let widths =
      G.map_vertex graph ~f:(fun v -> Map.length v.Node.state)
      |> List.sort ~compare:[%compare: int]
      |> Array.of_list
    in
    Stats.
      {
        n_nodes = G.nb_vertex graph;
        n_covered =
          G.filter_vertex graph ~f:(fun v -> v.Node.covered) |> List.length;
        n_refuted = !n_refuted;
        min_width = widths.(0);
        max_width = widths.(Array.length widths - 1);
        median_width = widths.(Array.length widths / 2);
        sat = (match status with `Sat -> true | `Unsat -> false);
      }
