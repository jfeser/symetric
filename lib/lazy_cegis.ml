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
end

module Abs_state = struct
  type t = bool Map.M(Int).t [@@deriving compare, hash, sexp]

  let top = Map.empty (module Int)

  let pp =
    Fmt.using (fun m ->
        Map.to_alist m |> List.map ~f:(fun (k, v) -> (Bool.to_int v, k)))
    @@ Fmt.list ~sep:(Fmt.any " ")
    @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "_%d")
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

module Node = struct
  type t = { id : int; state : Abs_state.t; op : Op.t; cost : int }

  let pp fmt ({ state; op; _ } : t) =
    Fmt.pf fmt "%a %a" Op.pp op Abs_state.pp state

  let equal n n' = [%compare.equal: int] n.id n'.id

  let compare n n' = [%compare: int] n.id n'.id

  let hash n = [%hash: int] n.id
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
    include Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node) (E)

    let graph_attributes _ = []

    let default_vertex_attributes _ = []

    let vertex_name n = Fmt.str "%d" n.Node.id

    let vertex_attributes n = [ `HtmlLabel (Fmt.str "%a" Node.pp n) ]

    let get_subgraph _ = None

    let default_edge_attributes _ = []

    let edge_attributes (_, i, _) = [ `Label (sprintf "%d" i) ]
  end

  include G
  include Graph.Graphviz.Dot (G)
end

exception
  Done of
    ([ `Input of State.t | `App of [ `Union | `Inter | `Sub ] * 't list ] as 't)

(** Refine an abstract value 'a' so that it contains a concrete value 'c' and
   excludes a concrete value 'c''. *)
let refine c a c' =
  assert (Array.length c = Array.length c');
  let len = Array.length c in
  let rec loop i =
    if i >= len then failwith "Could not refine"
    else if Bool.(c.(i) <> c'.(i)) then Map.add_exn a ~key:i ~data:c.(i)
    else loop (i + 1)
  in
  loop 0

let refine_children conc_inputs abs_inputs bad_output f =
  let conc_inputs = Array.of_list conc_inputs
  and abs_inputs = Array.of_list abs_inputs in
  let n_inputs = Array.length conc_inputs in
  assert (n_inputs = Array.length abs_inputs && n_inputs > 0);
  let len_inputs = Array.length conc_inputs.(0) in
  let rec loop i j =
    if i >= n_inputs || j >= len_inputs then failwith "Could not refine"
    else (
      abs_inputs.(i) <- Map.set abs_inputs.(i) ~key:j ~data:conc_inputs.(i).(j);
      if contains (f abs_inputs) bad_output then
        if j = len_inputs - 1 then loop (i + 1) 0 else loop i (j + 1)
      else abs_inputs )
  in
  loop 0 0 |> Array.to_list

let%expect_test "" =
  let t = true and f = false in
  refine_children [ [| t; f |]; [| t; f |] ]
    [
      Map.of_alist_exn (module Int) [ (0, t) ]; Map.of_alist_exn (module Int) [];
    ]
    [| t; t |]
    (function [| x; y |] -> union x y | _ -> assert false)
  |> Fmt.pr "%a" @@ Fmt.Dump.list Abs_state.pp;
  [%expect {| [1_0 0_1; 1_0 0_1] |}]

let fill graph cache fresh cost =
  let arg_cost = cost - 1 in
  let module Part = Combinat.Partition in
  let module Perm = Combinat.Permutation.Of_list in
  Part.create ~n:arg_cost ~parts:2
  |> Part.iter ~f:(fun arg_costs ->
         let arg_costs =
           List.init (Bigarray.Array1.dim arg_costs) ~f:(fun i -> arg_costs.{i})
         in
         Perm.(create arg_costs |> to_list)
         |> List.dedup_and_sort ~compare:[%compare: int list]
         |> List.iter ~f:(fun arg_costs ->
                match arg_costs with
                | [ c; c' ] ->
                    Hashtbl.find_multi cache c
                    |> List.iter ~f:(fun a ->
                           Hashtbl.find_multi cache c'
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
                                      let node =
                                        Node.
                                          {
                                            id = Fresh.int fresh;
                                            state;
                                            cost;
                                            op = (op :> Op.t);
                                          }
                                      in
                                      G.add_vertex graph node;
                                      G.add_edge_e graph (node, 0, a);
                                      G.add_edge_e graph (node, 1, a'))))
                | _ -> failwith "Unexpected costs"))

let rec to_program graph node =
  `Apply (node.Node.op, G.succ graph node |> List.map ~f:(to_program graph))

let rec conc_eval = function
  | `Apply (`Input x, _) -> x
  | `Apply (`Union, [ x; y ]) ->
      Array.map2_exn (conc_eval x) (conc_eval y) ~f:( || )
  | `Apply (`Inter, [ x; y ]) ->
      Array.map2_exn (conc_eval x) (conc_eval y) ~f:( && )
  | `Apply (`Sub, [ x; y ]) ->
      Array.map2_exn (conc_eval x) (conc_eval y) ~f:(fun a b -> a && not b)
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
  let rec process = function
    | v :: vs ->
        let worklist =
          ( G.pred g v
          |> List.filter_map ~f:(fun v' ->
                 let old_state = v'.Node.state in
                 let new_state = eval g v' in
                 if [%compare.equal: Abs_state.t] old_state new_state then None
                 else Some (update_state g v' new_state)) )
          @ vs
        in
        process worklist
    | [] -> ()
  in
  process [ node ]

let rec strengthen graph node bad_out =
  assert (contains node.Node.state bad_out);
  let inputs = G.succ graph node in
  let conc_inputs =
    List.map ~f:(to_program graph) inputs |> List.map ~f:conc_eval
  and abs_inputs = List.map inputs ~f:(fun n -> n.Node.state) in
  let f =
    match node.op with
    | `Union -> fun args -> union args.(0) args.(1)
    | `Inter -> fun args -> inter args.(0) args.(1)
    | `Sub -> fun args -> sub args.(0) args.(1)
    | _ -> assert false
  in
  let refined_inputs = refine_children conc_inputs abs_inputs bad_out f in
  List.iter2_exn inputs refined_inputs ~f:(fun input_node refined_state ->
      if not ([%compare.equal: Abs_state.t] input_node.state refined_state) then
        prune graph @@ update_state graph input_node refined_state)

let%expect_test "" =
  let g = G.create () in
  let v1 =
    Node.
      {
        id = 0;
        state = Map.singleton (module Int) 0 true;
        op = `Input [| true; true |];
        cost = 1;
      }
  and v2 =
    Node.
      {
        id = 1;
        state = Map.singleton (module Int) 1 true;
        op = `Input [| true; true |];
        cost = 1;
      }
  and v3 =
    Node.{ id = 3; state = Map.empty (module Int); op = `Inter; cost = 1 }
  in
  G.add_edge_e g (v3, 0, v1);
  G.add_edge_e g (v3, 1, v2);
  replace_vertex g v3 { v3 with op = `Union };
  G.output_graph stdout g;
  [%expect {|
    digraph G {
      0 [label=<in 1_0>, ];
      1 [label=<in 1_1>, ];
      3 [label=<or >, ];


      3 -> 0 [label="0", ];
      3 -> 1 [label="1", ];

      } |}]

let%expect_test "" =
  let g = G.create () in
  let v1 =
    Node.
      {
        id = 0;
        state = Map.singleton (module Int) 0 true;
        op = `Input [| true; true |];
        cost = 1;
      }
  and v2 =
    Node.
      {
        id = 1;
        state = Map.singleton (module Int) 1 true;
        op = `Input [| true; true |];
        cost = 1;
      }
  and v3 =
    Node.{ id = 3; state = Map.empty (module Int); op = `Inter; cost = 1 }
  in
  G.add_edge_e g (v3, 0, v1);
  G.add_edge_e g (v3, 1, v2);
  strengthen g v3 [| false; false |];
  Out_channel.flush stdout;
  G.output_graph stdout g;
  [%expect {|
    digraph G {
      0 [label=<in 1_0>, ];
      1 [label=<in 1_0 1_1>, ];
      3 [label=<and 1_0>, ];


      3 -> 0 [label="0", ];
      3 -> 1 [label="1", ];

      } |}]

let synth inputs output =
  let graph = G.create ()
  and cache = Hashtbl.create (module Int)
  and fresh = Fresh.create () in

  (* Add inputs to the state space graph. *)
  List.iter inputs ~f:(fun input ->
      if State.O.(input = output) then raise @@ Done (`Input input)
      else
        let state = refine input Abs_state.top output in
        let node =
          Node.{ id = Fresh.int fresh; state; cost = 1; op = `Input input }
        in
        G.add_vertex graph node;
        Hashtbl.add_multi cache ~key:1 ~data:node);

  (* Test filling *)
  fill graph cache fresh 3;

  Fmt.pr "%a" G.fprint_graph graph

let () =
  let conv l =
    List.map l ~f:(fun i -> if i = 0 then false else true) |> Array.of_list
  in
  let inputs =
    List.map ~f:conv [ [ 0; 1; 1; 0 ]; [ 1; 1; 0; 0 ]; [ 0; 0; 0; 1 ] ]
  in
  let output = conv [ 0; 1; 0; 1 ] in
  synth inputs output
