open! Core

let enable_dump = ref false

let value_exn x = Option.value_exn x

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

  let pp = Fmt.(array ~sep:(any " ") bool)
end

module Abs_state = struct
  type t = bool Map.M(Int).t [@@deriving compare, hash, sexp]

  let top = Map.empty (module Int)

  let pp =
    Fmt.using (fun m ->
        Map.to_alist m |> List.map ~f:(fun (k, v) -> (Bool.to_int v, k)))
    @@ Fmt.list ~sep:(Fmt.any " ")
    @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "_%d")

  let graphviz_pp =
    Fmt.using (fun m ->
        Map.to_alist m |> List.map ~f:(fun (k, v) -> (Bool.to_int v, k)))
    @@ Fmt.list ~sep:(Fmt.any " ")
    @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "<sub>%d</sub>")

  let meet =
    Map.merge ~f:(fun ~key:_ ->
      function
      | `Left x | `Right x -> Some x
      | `Both (x, x') ->
          assert (Bool.(x = x'));
          Some x)

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

let mk_id =
  let ctr = ref 0 in
  fun () ->
    incr ctr;
    !ctr

module Args_node = struct
  type t = { id : int; op : Op.t } [@@deriving compare, hash, sexp]

  let graphviz_pp fmt { op; _ } = Op.pp fmt op

  let create op = { id = mk_id (); op }

  let id { id; _ } = id

  let op { op; _ } = op
end

module State_node0 = struct
  let time = ref 0

  type t = {
    id : int;
    cost : int;
    cstate : State.t;
    mutable state : Abs_state.t;
    mutable covered : bool;
    mutable last_mod_time : int;
  }

  let id { id; _ } = id

  let state { state; _ } = state

  let cstate { cstate; _ } = cstate

  let set_state n s = n.state <- s

  let cover n =
    if not n.covered then (
      incr time;
      n.covered <- true;
      n.last_mod_time <- !time )

  let uncover n =
    if n.covered then (
      incr time;
      n.covered <- false;
      n.last_mod_time <- !time )

  let pp fmt { state; _ } = Abs_state.pp fmt state

  let graphviz_pp fmt { state; last_mod_time; _ } =
    Fmt.pf fmt "%a %d" Abs_state.graphviz_pp state last_mod_time

  let hash n = [%hash: int] n.id

  let hash_fold_t state n = [%hash_fold: int] state n.id

  let equal n n' = [%compare.equal: int] n.id n'.id

  let compare n n' = [%compare: int] n.id n'.id
end

module Node = struct
  type t = Args of Args_node.t | State of State_node0.t
  [@@deriving compare, hash]

  let equal = [%compare.equal: t]

  let id = function Args x -> Args_node.id x | State x -> State_node0.id x

  let to_args = function Args x -> x | _ -> failwith "expected an args node"
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
    include Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node) (E)

    let graph_attributes _ = []

    let default_vertex_attributes _ = []

    let vertex_name n = Fmt.str "%d" @@ Node.id n

    let vertex_attributes = function
      | Node.State x -> [ `HtmlLabel (Fmt.str "%a" State_node0.pp x) ]
      | Args _ -> [ `Shape `Point ]

    let get_subgraph _ = None

    let default_edge_attributes _ = []

    let edge_attributes (_, i, _) = [ `Label (sprintf "%d" i) ]
  end

  include G
  include Graph.Graphviz.Dot (G)

  let iter_state_vertex g ~f =
    iter_vertex (function State x -> f x | _ -> ()) g

  let filter_vertex g ~f =
    fold_vertex (fun x xs -> if f x then x :: xs else xs) g []

  let filter_map_vertex g ~f =
    fold_vertex
      (fun x xs -> match f x with Some x' -> x' :: xs | None -> xs)
      g []

  let map_vertex g ~f = fold_vertex (fun x xs -> f x :: xs) g []

  let exists_vertex g ~f = fold_vertex (fun x xs -> f x || xs) g false

  let find_vertex g ~f =
    fold_vertex
      (fun x acc -> if Option.is_none acc && f x then Some x else acc)
      g None

  let children g v =
    succ g (State v)
    |> List.map ~f:(fun v' ->
           let a = Node.to_args v' in
           let op = Args_node.op a in
           let args =
             succ_e g v'
             |> List.sort ~compare:(fun (_, x, _) (_, x', _) ->
                    [%compare: int] x x')
             |> List.map ~f:(function
                  | _, _, Node.State v -> v
                  | _ -> assert false)
           in
           (op, args))

  (** The set of nodes that depend on a state 'v'. *)
  let depends g v =
    pred g (State v)
    |> List.concat_map ~f:(pred g)
    |> List.dedup_and_sort ~compare:[%compare: Node.t]
    |> List.map ~f:(function Node.State x -> x | Args _ -> assert false)

  let ensure_vertex g v = if not (mem_vertex g v) then add_vertex g v

  let ensure_edge_e g e = if not (mem_edge_e g e) then add_edge_e g e
end

let ceval' op args =
  match (op, args) with
  | `Input x, _ -> x
  | `Union, [ x; y ] -> Array.map2_exn x.State_node0.cstate y.cstate ~f:( || )
  | `Inter, [ x; y ] -> Array.map2_exn x.cstate y.cstate ~f:( && )
  | `Sub, [ x; y ] ->
      Array.map2_exn x.cstate y.cstate ~f:(fun a b -> a && not b)
  | _ -> assert false

let eval op args =
  match (op, args) with
  | `Input state, _ -> Abs_state.lift state
  | `Union, [ x; y ] -> union x y
  | `Inter, [ x; y ] -> inter x y
  | `Sub, [ x; y ] -> sub x y
  | _ -> failwith "Unexpected args"

module Program = struct
  module T = struct
    type t = [ `Apply of Op.t * t list ] [@@deriving compare, hash, sexp]
  end

  let rec ceval (`Apply (op, args)) =
    match (op, args) with
    | `Input x, _ -> x
    | `Union, [ x; y ] -> Array.map2_exn (ceval x) (ceval y) ~f:( || )
    | `Inter, [ x; y ] -> Array.map2_exn (ceval x) (ceval y) ~f:( && )
    | `Sub, [ x; y ] ->
        Array.map2_exn (ceval x) (ceval y) ~f:(fun a b -> a && not b)
    | _ -> assert false

  let rec size (`Apply (op, args)) = 1 + List.sum (module Int) args ~f:size

  include T
  include Comparator.Make (T)
end

let prune g (nodes : State_node0.t list) =
  let open State_node0 in
  Fmt.epr "Pruning %d nodes.\n" (List.length nodes);
  let min_mod_time =
    Option.value_exn
      ( List.map nodes ~f:(fun n -> n.last_mod_time)
      |> List.min_elt ~compare:[%compare: int] )
  in
  G.iter_state_vertex
    ~f:(fun v -> if v.last_mod_time > min_mod_time then uncover v)
    g;
  Fmt.epr "Prune: min mod time %d.\n" min_mod_time;

  let rec process = function
    | v :: vs ->
        let work =
          List.filter_map (G.depends g v) ~f:(fun v' ->
              let old = v'.State_node0.state
              and new_ =
                List.map (G.children g v') ~f:(fun (op, args) ->
                    eval op (List.map args ~f:State_node0.state))
                |> List.reduce_exn ~f:Abs_state.meet
              in
              [%test_pred: Abs_state.t * Abs_state.t]
                (fun (old, new_) -> Abs_state.is_subset_a old ~of_:new_)
                (old, new_);
              if [%compare.equal: Abs_state.t] old new_ then None
              else (
                set_state v' new_;
                Some v' ))
        in
        process (work @ vs)
    | [] -> ()
  in
  process nodes

let rec refine_child graph node op children =
  Fmt.epr "Children of %a\n" State_node0.pp node;

  let open State_node0 in
  let input_nodes = List.to_array children in
  if Array.is_empty input_nodes then []
  else
    let conc_inputs = Array.map input_nodes ~f:cstate
    and old_inputs = Array.map input_nodes ~f:state in
    let n = Array.length conc_inputs and k = Array.length conc_inputs.(0) in

    (* The old state contains the concrete behavior. *)
    assert (
      Array.for_alli old_inputs ~f:(fun i old -> contains old conc_inputs.(i))
    );

    let strong_enough () =
      Abs_state.is_subset_a node.state
        ~of_:(eval op @@ List.map children ~f:State_node0.state)
    in

    let changed = Array.create n false in
    let rec loop i j =
      if i >= n || j >= k then failwith "Could not refine";

      if not (Map.mem input_nodes.(i).state j) then (
        set_state input_nodes.(i)
          (Map.add_exn input_nodes.(i).state ~key:j ~data:conc_inputs.(i).(j));
        changed.(i) <- true );

      if not (strong_enough ()) then
        if j = k - 1 then loop (i + 1) 0 else loop i (j + 1)
    in
    loop 0 0;

    (* The new input states produce an output that refines the abstract output.
       *)
    assert (strong_enough ());

    (* The new state refines the old state. *)
    assert (
      Array.for_alli old_inputs ~f:(fun i old ->
          let new_ = input_nodes.(i).state in
          Abs_state.is_subset_a old ~of_:new_) );

    (* The new state contains the concrete behavior. *)
    assert (
      Array.for_alli conc_inputs ~f:(fun i conc ->
          let new_ = input_nodes.(i).state in
          contains new_ conc) );

    let to_prune =
      Array.filter_mapi changed ~f:(fun i c ->
          if c then Some input_nodes.(i) else None)
      |> Array.to_list
    in
    to_prune @ List.concat_map to_prune ~f:(refine_children graph)

and refine_children graph node =
  G.children graph node
  |> List.concat_map ~f:(fun (op, args) -> refine_child graph node op args)

module State_node = struct
  include State_node0

  let rec choose_program graph node =
    match G.children graph node with
    | (op, args) :: _ -> `Apply (op, List.map args ~f:(choose_program graph))
    | _ -> failwith "expected arguments"

  let create ?(covered = false) ~state ~cost ~op graph children =
    let cstate = ceval' op children in

    let mk_args state op children =
      let args = Node.Args (Args_node.create op) in
      G.ensure_vertex graph args;
      G.ensure_edge_e graph (state, -1, args);
      List.iteri children ~f:(fun i x ->
          G.ensure_edge_e graph (args, i, Node.State x));
      Some state
    in

    match
      G.find_vertex graph ~f:(function
        | State v -> [%compare.equal: State.t] v.cstate cstate
        | Args _ -> false)
    with
    | Some state ->
        let args = Node.Args (Args_node.create op) in
        G.ensure_vertex graph args;
        G.ensure_edge_e graph (state, -1, args);
        List.iteri children ~f:(fun i x ->
            G.ensure_edge_e graph (args, i, Node.State x));
        Some state
    | None ->
        incr time;
        let state =
          Node.State
            {
              id = mk_id ();
              cost;
              state;
              cstate;
              covered;
              last_mod_time = !time;
            }
        and args = Node.Args (Args_node.create op) in
        G.ensure_vertex graph state;
        G.ensure_vertex graph args;
        G.ensure_edge_e graph (state, -1, args);
        List.iteri children ~f:(fun i x ->
            G.ensure_edge_e graph (args, i, Node.State x));
        Some state

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
      G.filter_map_vertex
        ~f:(function
          | State v when (not v.covered) && v.cost = c -> Some v | _ -> None)
        graph
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
                      |> List.iter ~f:(fun (a : State_node.t) ->
                             of_cost c'
                             |> List.iter ~f:(fun (a' : State_node.t) ->
                                    List.iter [ `Union; `Inter; `Sub ]
                                      ~f:(fun op ->
                                        let state =
                                          match op with
                                          | `Union -> union a.state a'.state
                                          | `Inter -> inter a.state a'.state
                                          | `Sub -> sub a.state a'.state
                                        in
                                        let did_add =
                                          State_node.create ~state ~cost
                                            ~op:(op :> Op.t)
                                            graph [ a; a' ]
                                          |> Option.is_some
                                        in
                                        added := !added || did_add)))
                  | _ -> failwith "Unexpected costs"));
    !added

let refine graph (node : State_node.t) bad =
  let conc = node.cstate and old = node.state in

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

  State_node.set_state node new_;
  [ node ]

let rec strengthen graph (node : State_node.t) bad_out =
  assert (contains node.state bad_out);
  let to_prune = refine graph node bad_out in
  let to_prune' = refine_children graph node in
  assert (not (contains node.state bad_out));
  to_prune @ to_prune'

let%expect_test "" =
  let g = G.create () in
  State_node.create_exn
    ~state:(Map.singleton (module Int) 0 true)
    ~op:(`Input [| true; true |])
    ~cost:1 g []
  |> ignore;
  State_node.create_exn
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

    let vertex_name n = Fmt.str "%d" @@ Node.id n

    let vertex_attributes = function
      | Node.State n ->
          [ `HtmlLabel (Fmt.str "%a" State_node.graphviz_pp n) ]
          @
          if n.covered then [ `Style `Dotted ]
          else [] @ if contains n.state output then [ `Style `Bold ] else []
      | Args n ->
          [ `HtmlLabel (Fmt.str "%a" Args_node.graphviz_pp n); `Shape `Box ]

    let get_subgraph _ = None

    let default_edge_attributes _ = []

    let edge_attributes (_, i, _) =
      if i >= 0 then [ `Label (sprintf "%d" i) ] else []
  end) in
  let dump () =
    if !enable_dump then (
      Out_channel.with_file (sprintf "out%d.dot" !step) ~f:(fun ch ->
          Viz.output_graph ch graph);
      Out_channel.flush stdout;
      incr step )
  in

  let n_refuted = ref 0 in

  let update_covers () =
    G.filter_map_vertex graph ~f:(function
      | State v when not v.covered -> Some v
      | _ -> None)
    |> List.iter ~f:(fun (v : State_node.t) ->
           if
             G.exists_vertex graph ~f:(function
               | State v' ->
                   (not v'.covered) && v'.cost <= v.cost
                   && (not ([%compare.equal: State_node.t] v v'))
                   && Abs_state.is_subset_a v'.state ~of_:v.state
               | _ -> false)
           then (
             Fmt.epr "Covering: %a\n" State_node.pp v;
             State_node.cover v ))
  in

  let refute () =
    let contains_output =
      G.filter_map_vertex graph ~f:(function
        | State v when (not v.covered) && contains v.state output -> Some v
        | _ -> None)
    in
    List.iter contains_output ~f:(fun (v : State_node.t) ->
        if [%compare.equal: State.t] v.cstate output then raise (Done `Sat)
        else (
          incr n_refuted;
          (* Fmt.epr "Refuting: %a\n" Sexp.pp_hum
           *   ([%sexp_of: Program.t] (State_node.choose_program graph v)); *)
          let to_prune = strengthen graph v output in
          dump ();
          prune graph to_prune;
          dump () ));
    not (List.is_empty contains_output)
  in

  let rec loop cost =
    if cost > max_cost then raise (Done `Unsat);
    let rec strengthen_and_cover () =
      update_covers ();
      dump ();
      let did_strengthen = refute () in
      if did_strengthen then strengthen_and_cover ()
    in
    strengthen_and_cover ();

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
      State_node.create ~state ~cost:1 ~op:(`Input input) graph [] |> ignore);
  dump ();

  try loop 1
  with Done status ->
    let widths =
      G.filter_map_vertex graph ~f:(function
        | State v -> Some (Map.length v.state)
        | _ -> None)
      |> List.sort ~compare:[%compare: int]
      |> Array.of_list
    in
    ( graph,
      Stats.
        {
          n_nodes = G.nb_vertex graph;
          n_covered =
            G.filter_map_vertex graph ~f:(function
              | State v when v.covered -> Some ()
              | _ -> None)
            |> List.length;
          n_refuted = !n_refuted;
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
  let term = Grammar.sample ~state "p" g in
  let rec to_prog = function
    | App (func, args) ->
        let op =
          match func with
          | "and" -> `Inter
          | "or" -> `Union
          | "diff" -> `Sub
          | _ -> (
              match
                List.Assoc.find ~equal:[%compare.equal: string] named_inputs
                  func
              with
              | Some i -> `Input i
              | None -> failwith "unexpected function" )
        in
        let args = List.map args ~f:to_prog in
        `Apply (op, args)
    | _ -> failwith "unexpected term"
  in
  to_prog (term :> Untyped_term.t)

let check_search_space ?(n = 100_000) inputs graph =
  let rec loop i =
    if i > n then Ok ()
    else
      let prog = sample inputs in
      let cstate = Program.ceval prog in
      match
        G.find_vertex graph ~f:(function
          | State v -> contains v.state cstate
          | _ -> false)
      with
      | Some v -> loop (i + 1)
      | None ->
          Fmt.epr "Missed program %a with size %d and state %a\n" Sexp.pp
            ([%sexp_of: Program.t] prog)
            (Program.size prog) State.pp cstate;
          Error cstate
  in
  loop 0
