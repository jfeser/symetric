open! Core

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
    mutable state : Abs_state.t;
    mutable covered : bool;
    mutable last_mod_time : int;
  }

  let hash n = n.id

  let hash_fold_t state n = [%hash_fold: int] state (hash n)

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

  let equal n n' = [%compare.equal: int] n.id n'.id

  let compare n n' = [%compare: int] n.id n'.id
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
    | `Both (true, false) -> Some true
    | `Both (true, true) -> Some false
    | `Both (false, _) -> Some false
    | `Left false | `Right true -> Some false
    | _ -> None)

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

  let exists_vertex g ~f = fold_vertex (fun x xs -> f x || xs) g false
end

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
    `Apply
      ( node.op,
        G.succ_e graph node
        |> List.sort ~compare:(fun (_, x, _) (_, x', _) -> [%compare: int] x x')
        |> List.map ~f:(fun (_, _, v) -> v)
        |> List.map ~f:(to_program graph) )

  let create ?(covered = false) ~state ~cost ~op graph children =
    let prog = `Apply (op, List.map children ~f:(to_program graph)) in
    if
      G.exists_vertex graph ~f:(fun v ->
          [%compare.equal: Program.t] prog (to_program graph v))
    then None
    else (
      incr time;
      let node =
        {
          id = G.nb_vertex graph;
          op;
          cost;
          state;
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
                                        (* Fmt.pr "%a %a %a %b\n" Op.pp op Node.pp
                                         *   a Node.pp a' did_add; *)
                                        added := !added || did_add)))
                  | _ -> failwith "Unexpected costs"));
    !added

let rec ceval g v =
  let args = G.succ g v in
  match (v.op, args) with
  | `Input x, _ -> x
  | `Union, [ x; y ] -> Array.map2_exn (ceval g x) (ceval g y) ~f:( || )
  | `Inter, [ x; y ] -> Array.map2_exn (ceval g x) (ceval g y) ~f:( && )
  | `Sub, [ x; y ] ->
      Array.map2_exn (ceval g x) (ceval g y) ~f:(fun a b -> a && not b)
  | _ -> assert false

let eval g v =
  let args = G.succ g v in
  match (v.op, args) with
  | `Input _, _ -> v.state
  | `Union, [ v'; v'' ] -> union v'.state v''.state
  | `Inter, [ v'; v'' ] -> inter v'.state v''.state
  | `Sub, [ v'; v'' ] -> sub v'.state v''.state
  | _ -> failwith "Unexpected args"

let replace_vertex g ~old ~new_ =
  let in_edges = G.pred_e g old and out_edges = G.succ_e g old in
  G.remove_vertex g old;
  G.add_vertex g new_;
  List.iter in_edges ~f:(fun (v, x, _) -> G.add_edge_e g (v, x, new_));
  List.iter out_edges ~f:(fun (_, x, v) -> G.add_edge_e g (new_, x, v))

let update_state g v state =
  let v' = Node.{ v with state } in
  replace_vertex g ~old:v ~new_:v';
  v'

let prune g node =
  G.iter_vertex
    (fun v -> if v.last_mod_time > node.Node.last_mod_time then Node.uncover v)
    g;

  let rec process = function
    | v :: vs ->
        let worklist =
          ( G.pred g v
          |> List.filter_map ~f:(fun v' ->
                 let old = v'.Node.state in
                 let new_ = eval g v' in
                 if [%compare.equal: Abs_state.t] old new_ then None
                 else (
                   Node.set_state v' new_;
                   Some v' )) )
          @ vs
        in
        process worklist
    | [] -> ()
  in
  process [ node ]

let refine graph node bad =
  let conc = ceval graph node in
  assert (
    (not ([%compare.equal: State.t] bad conc))
    && contains node.state bad && contains node.state conc );
  let len = Array.length conc in
  let rec loop i =
    if i >= len then failwith "Could not refine"
    else if Bool.(conc.(i) <> bad.(i)) then (
      try Map.add_exn node.state ~key:i ~data:conc.(i)
      with _ ->
        Fmt.pr "%a %a %a\n" Node.pp node State.pp conc State.pp bad;
        exit 1 )
    else loop (i + 1)
  in
  Node.set_state node (loop 0);
  [ node ]

let refine_children graph node f =
  let input_nodes = G.succ graph node in
  if List.is_empty input_nodes then []
  else
    let conc_inputs = List.map input_nodes ~f:(ceval graph) |> Array.of_list
    and abs_inputs =
      List.map input_nodes ~f:(fun n -> n.Node.state) |> Array.of_list
    in
    let n_inputs = Array.length conc_inputs in
    let len_inputs = Array.length conc_inputs.(0) in
    let rec loop i j =
      if i >= n_inputs || j >= len_inputs then failwith "Could not refine"
      else (
        abs_inputs.(i) <-
          Map.set abs_inputs.(i) ~key:j ~data:conc_inputs.(i).(j);
        if not (Abs_state.is_subset_a node.state ~of_:(f abs_inputs)) then
          if j = len_inputs - 1 then loop (i + 1) 0 else loop i (j + 1) )
    in
    loop 0 0;
    List.map2_exn (Array.to_list abs_inputs) input_nodes ~f:(fun s v ->
        if not ([%compare.equal: Abs_state.t] v.Node.state s) then (
          Node.set_state v s;
          Some v )
        else None)
    |> List.filter_map ~f:Fun.id

(* let%expect_test "" =
 *   let t = true and f = false in
 *   refine_children [ [| t; f |]; [| t; f |] ]
 *     [
 *       Map.of_alist_exn (module Int) [ (0, t) ]; Map.of_alist_exn (module Int) [];
 *     ]
 *     (Map.of_alist_exn (module Int) [ (0, t); (1, t) ])
 *     (function [| x; y |] -> union x y | _ -> assert false)
 *   |> Fmt.pr "%a" @@ Fmt.Dump.list Abs_state.pp;
 *   [%expect {| [1_0 0_1; 1_0 0_1] |}] *)

let rec strengthen graph node bad_out =
  assert (contains node.Node.state bad_out);
  let f =
    match node.op with
    | `Union -> fun args -> union args.(0) args.(1)
    | `Inter -> fun args -> inter args.(0) args.(1)
    | `Sub -> fun args -> sub args.(0) args.(1)
    | `Input s -> fun _ -> Abs_state.lift s
  in
  let to_prune = refine graph node bad_out in
  let to_prune' = refine_children graph node f in
  List.iter (to_prune @ to_prune') ~f:(prune graph)

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
  |> List.iter ~f:(Fmt.pr "%a\n" Node.pp);
  [%expect {|
    and
    in 1_1
    in 1_0 |}]

let synth inputs output =
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
    if !step >= 100 then exit 1;
    Out_channel.with_file (sprintf "out%d.dot" !step) ~f:(fun ch ->
        Viz.output_graph ch graph);
    Out_channel.flush stdout;
    incr step
  in

  let rec loop cost =
    let rec iloop () =
      G.filter_vertex graph ~f:(fun v -> not v.covered)
      |> List.iter ~f:(fun (v : Node.t) ->
             if
               G.exists_vertex graph ~f:(fun (v' : Node.t) ->
                   (not v'.covered) && v'.cost <= v.cost
                   && (not ([%compare.equal: Node.t] v v'))
                   && Abs_state.is_subset_a v'.state ~of_:v.state)
             then (
               Fmt.pr "Covering: %a\n" Node.pp v;
               Node.cover v ));
      dump ();

      let did_strengthen = ref false in
      G.filter_vertex graph ~f:(fun v -> not v.covered)
      |> List.iter ~f:(fun v ->
             if contains v.Node.state output then
               if [%compare.equal: State.t] (ceval graph v) output then
                 failwith "sat"
               else (
                 Fmt.pr "Refuting: %a\n" Sexp.pp_hum
                   ([%sexp_of: Program.t] (Node.to_program graph v));
                 strengthen graph v output;
                 did_strengthen := true ));
      dump ();
      if !did_strengthen then iloop ()
    in

    iloop ();

    let changed = fill graph cost in
    Fmt.pr "Changed: %b Cost: %d\n" changed cost;
    if changed then dump ();
    let cost = if changed then cost else cost + 1 in
    loop cost
  in

  (* Add inputs to the state space graph. *)
  List.iter inputs ~f:(fun input ->
      Node.create
        ~state:(Map.empty (module Int))
        ~cost:1 ~op:(`Input input) graph []
      |> ignore);
  dump ();

  loop 1
