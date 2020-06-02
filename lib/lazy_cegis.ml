open! Core

module State = struct
  module T = struct
    type t = bool array [@@deriving compare, sexp]
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
    @@ Fmt.pair Fmt.int (Fmt.fmt "<sub>%d</sub>")
end

module Op = struct
  type t = [ `Input | `Union | `Inter | `Sub ] [@@deriving compare, hash, sexp]

  let pp fmt op =
    let str =
      match op with
      | `Input -> "in"
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

let contains a c =
  let arr = Array.of_list c in
  Map.for_alli a ~f:(fun ~key:i ~data:v -> Bool.(arr.(i) = v))

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
  loop 0 0

let%expect_test "" =
  let t = true and f = false in
  refine_children [ [| t; f |]; [| t; f |] ]
    [
      Map.of_alist_exn (module Int) [ (0, t) ]; Map.of_alist_exn (module Int) [];
    ]
    [ t; t ]
    (function [| x; y |] -> union x y | _ -> assert false)
  |> Fmt.pr "%a" @@ Fmt.Dump.array Abs_state.pp;
  [%expect {| [|1<sub>0</sub> 0<sub>1</sub>; 1<sub>0</sub> 0<sub>1</sub>|] |}]

let fill graph cache fresh cost =
  let arg_cost = cost - 1 in
  Combinat.Partition.(
    create ~n:arg_cost ~parts:2
    |> iter ~f:(fun arg_costs ->
           let arg_costs =
             List.init (Bigarray.Array1.dim arg_costs) ~f:(fun i ->
                 arg_costs.{i})
           in
           Combinat.Permutation.Of_list.(create arg_costs |> to_list)
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
                  | _ -> failwith "Unexpected costs")))

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
          Node.{ id = Fresh.int fresh; state; cost = 1; op = `Input }
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
