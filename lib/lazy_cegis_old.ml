open! Core

module Seq = struct
  include Sequence

  let of_array a = init (Array.length a) ~f:(fun i -> a.(i))
end

let value_exn x = Option.value_exn x

let enable_dump = ref false

let refine_strategy : [ `First | `Random | `Pareto ] ref = ref `First

let max_cost = ref 10

let n_refuted = ref 0

module Conc = struct
  module T = struct
    type t = bool array [@@deriving compare, equal, sexp]

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

module type ABS = sig
  type t [@@deriving compare, equal, hash, sexp]

  include Comparator.S with type t := t

  val top : t

  val pp : t Fmt.t

  val graphviz_pp : t Fmt.t

  val meet : t -> t -> t

  val is_subset_a : t -> of_:t -> bool

  val lift : Conc.t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val sub : t -> t -> t

  val mem : t -> int -> bool

  val add_exn : t -> int -> bool -> t

  val set : t -> int -> bool -> t

  val contains : t -> Conc.t -> bool

  val width : t -> int

  val of_list_exn : (int * bool) list -> t
end

module Map_abs : ABS with type t = bool Map.M(Int).t = struct
  module T = struct
    type t = bool Map.M(Int).t [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

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
    Map.merge ~f:(fun ~key:_ -> function
      | `Left x | `Right x -> Some x
      | `Both (x, x') ->
          assert (Bool.(x = x'));
          Some x)

  let is_subset_a s ~of_:s' =
    if Map.length s > Map.length s' then false
    else
      Map.fold2 s s' ~init:true ~f:(fun ~key ~data acc ->
          acc
          &&
          match data with
          | `Left _ -> false
          | `Right _ -> true
          | `Both (x, x') -> Bool.(x = x'))

  let lift s =
    Array.mapi s ~f:(fun i x -> (i, x))
    |> Array.to_list
    |> Map.of_alist_exn (module Int)

  let union =
    Map.merge ~f:(fun ~key:_ -> function
      | `Both (x, x') -> Some (x || x')
      | `Left true | `Right true -> Some true
      | `Left false | `Right false -> None)

  let inter =
    Map.merge ~f:(fun ~key:_ -> function
      | `Both (x, x') -> Some (x && x')
      | `Left false | `Right false -> Some false
      | `Left true | `Right true -> None)

  let sub =
    Map.merge ~f:(fun ~key:_ -> function
      | `Both (x, x') -> Some (x && not x')
      | `Left false | `Right true -> Some false
      | `Left true | `Right false -> None)

  let mem v i = Map.mem v i

  let set m k v = Map.set m ~key:k ~data:v

  let add_exn m k v =
    match Map.find m k with
    | Some v' ->
        if Bool.(v = v') then m
        else
          Error.create "Conflicting values for bit" k [%sexp_of: int]
          |> Error.raise
    | None -> set m k v

  let contains a c = Map.for_alli a ~f:(fun ~key:i ~data:v -> Bool.(c.(i) = v))

  let width = Map.length

  let of_list_exn l = Map.of_alist_exn (module Int) l
end

module Abs = Map_abs

module Op = struct
  type t = Input of Conc.t | Union | Inter | Sub
  [@@deriving compare, equal, hash, sexp]

  let pp fmt op =
    let str =
      match op with
      | Input _ -> "in"
      | Union -> "or"
      | Inter -> "and"
      | Sub -> "diff"
    in
    Fmt.pf fmt "%s" str

  let arity = function Input _ -> 0 | Union | Inter | Sub -> 2
end

let mk_id =
  let ctr = ref 0 in
  fun () ->
    incr ctr;
    !ctr

module Args_node0 = struct
  module T = struct
    type t = { id : int; op : Op.t }
    [@@deriving compare, equal, hash, sexp, show]
  end

  include T
  include Comparator.Make (T)

  let graphviz_pp fmt { op; id } = Fmt.pf fmt "%a<br/>id=%d" Op.pp op id

  let create op = { id = mk_id (); op }

  let id { id; _ } = id

  let op { op; _ } = op
end

module State_node0 = struct
  module T = struct
    type t = { id : int; cost : int; state : Abs.t [@ignore] }
    [@@deriving compare, equal, hash, sexp, show]
  end

  include T
  include Comparator.Make (T)

  let id { id; _ } = id

  let state { state; _ } = state

  let cost { cost; _ } = cost

  let graphviz_pp fmt { state; cost; id; _ } =
    Fmt.pf fmt "%a<br/>id=%d cost=%d" Abs.graphviz_pp state id cost
end

module Node = struct
  module T = struct
    type t = Args of Args_node0.t | State of State_node0.t
    [@@deriving compare, equal, hash, sexp, show]
  end

  include T
  include Comparator.Make (T)

  module O : Comparable.Infix with type t := t = Comparable.Make (T)

  let id = function Args x -> Args_node0.id x | State x -> State_node0.id x

  let to_args = function Args x -> Some x | _ -> None

  let to_args_exn x = Option.value_exn (to_args x)

  let to_state = function State x -> Some x | _ -> None

  let to_state_exn = function
    | State x -> x
    | Args x ->
        Error.create "Expected state." x [%sexp_of: Args_node0.t] |> Error.raise
end

module Edge = struct
  type t = int [@@deriving compare, equal, sexp]

  let default = -1
end

module G = struct
  module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node) (Edge)

  module Attr = struct
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
  include Graph.Graphviz.Dot (G) (Attr)
end

module Search_state = struct
  module Args_table_key = struct
    module T = struct
      type t = Op.t * State_node0.t list [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  module State_table_key = struct
    module T = struct
      type t = Abs.t * int [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  type t = {
    graph : G.t;
    args_table : Args_node0.t Hashtbl.M(Args_table_key).t;
    state_table : State_node0.t Hashtbl.M(State_table_key).t;
    cost_table : State_node0.t list array;
  }

  let create max_cost =
    {
      graph = G.create ();
      args_table = Hashtbl.create (module Args_table_key);
      state_table = Hashtbl.create (module State_table_key);
      cost_table = Array.create ~len:max_cost [];
    }

  let copy x =
    {
      graph = G.copy x.graph;
      args_table = Hashtbl.copy x.args_table;
      state_table = Hashtbl.copy x.state_table;
      cost_table = Array.copy x.cost_table;
    }

  let wrap f g = f g.graph

  module V = struct
    include Comparator.Make (struct
      type t = Node.t [@@deriving compare, sexp]
    end)

    include Container.Make0 (struct
      type nonrec t = t

      module Elt = struct
        type t = G.V.t [@@deriving equal]
      end

      let fold g ~init ~f = G.fold_vertex (fun v acc -> f acc v) g.graph init

      let iter = `Custom (fun g ~f -> G.iter_vertex f g.graph)

      let length = `Custom (wrap G.nb_vertex)
    end)

    let filter g ~f =
      fold ~f:(fun xs x -> if f x then x :: xs else xs) g ~init:[]

    let filter_map g ~f =
      fold
        ~f:(fun xs x -> match f x with Some x' -> x' :: xs | None -> xs)
        g ~init:[]

    let map g ~f = fold ~f:(fun xs x -> f x :: xs) g ~init:[]

    include G.V
  end

  module E = struct
    include Comparator.Make (struct
      type t = Node.t * int * Node.t [@@deriving compare, sexp]
    end)

    include Container.Make0 (struct
      type nonrec t = t

      module Elt = struct
        type t = G.E.t

        let equal e e' = G.E.compare e e' = 0
      end

      let fold g ~init ~f = G.fold_edges_e (fun v acc -> f acc v) g.graph init

      let iter = `Custom (fun g ~f -> G.iter_edges_e f g.graph)

      let length = `Custom (wrap G.nb_edges)
    end)

    let filter g ~f =
      fold ~f:(fun xs x -> if f x then x :: xs else xs) g ~init:[]

    let map g ~f = fold ~f:(fun xs x -> f x :: xs) g ~init:[]

    include G.E
  end

  let succ = wrap G.succ

  let pred = wrap G.pred

  let succ_e = wrap G.succ_e

  let pred_e = wrap G.pred_e

  let add_edge_e = wrap G.add_edge_e

  let children g v =
    G.succ g.graph (State v)
    |> List.map ~f:(fun v' ->
           let a = Node.to_args_exn v' in
           let op = Args_node0.op a in
           let args =
             succ_e g v'
             |> List.sort ~compare:(fun (_, x, _) (_, x', _) ->
                    [%compare: int] x x')
             |> List.map ~f:(function
                  | _, _, Node.State v -> v
                  | _ -> assert false)
           in
           (op, args))

  let remove_vertexes g vs =
    let vs = List.filter vs ~f:(G.mem_vertex g.graph) in
    let to_remove = Set.of_list (module V) vs in
    let remove_state v = not (Set.mem to_remove (State v)) in

    for i = 0 to Array.length g.cost_table - 1 do
      g.cost_table.(i) <- List.filter g.cost_table.(i) ~f:remove_state
    done;
    Set.iter to_remove ~f:(G.remove_vertex g.graph);
    Hashtbl.filter_inplace g.args_table ~f:(fun v ->
        not (Set.mem to_remove (Args v)));
    Hashtbl.filter_inplace g.state_table ~f:remove_state

  let remove_vertex x v = remove_vertexes x [ v ]

  let filter g ~f = remove_vertexes g @@ V.filter g ~f:(Fun.negate f)

  let states_of_cost g cost =
    let idx = cost - 1 in
    g.cost_table.(idx)

  let set_states_of_cost g cost states =
    let idx = cost - 1 in
    g.cost_table.(idx) <- states
end

module S = Search_state

let eval' op args =
  match (op, args) with
  | Op.Input state, _ -> Abs.lift state
  | Union, [ x; y ] -> Abs.union x y
  | Inter, [ x; y ] -> Abs.inter x y
  | Sub, [ x; y ] -> Abs.sub x y
  | _ -> failwith "Unexpected args"

let eval g a =
  let args =
    Search_state.succ_e g (Node.Args a)
    |> List.sort ~compare:(fun (_, i, _) (_, i', _) -> [%compare: int] i i')
    |> List.map ~f:(function
         | _, _, Node.State v -> v.state
         | _ -> failwith "expected a state node")
  in
  eval' a.op args

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
    match Hashtbl.find graph.Search_state.args_table (op, args) with
    | Some v -> None
    | None ->
        let args_n = { op; id = mk_id () } in
        let args_v = Node.Args args_n in
        List.iteri args ~f:(fun i v ->
            S.add_edge_e graph (args_v, i, Node.State v));
        Hashtbl.set graph.args_table (op, args) args_n;
        Some args_n
end

module State_node = struct
  include State_node0

  let rec choose_program graph node =
    match S.children graph node with
    | (op, args) :: _ -> `Apply (op, List.map args ~f:(choose_program graph))
    | _ -> failwith "expected arguments"

  let create ~state ~cost = { id = mk_id (); cost; state }

  let create_consed ~state ~cost (g : Search_state.t) =
    match Hashtbl.find g.state_table (state, cost) with
    | Some v -> `Stale v
    | None ->
        let v = { id = mk_id (); cost; state } in
        S.set_states_of_cost g cost (v :: S.states_of_cost g cost);
        Hashtbl.add_exn g.state_table (state, cost) v;
        `Fresh v

  let create_op ~state ~cost ~op g children =
    Args_node.create ~op g children
    |> Option.bind ~f:(fun args_v ->
           match create_consed ~state ~cost g with
           | `Fresh state_v ->
               S.add_edge_e g (Node.State state_v, -1, Node.Args args_v);
               Some state_v
           | `Stale state_v ->
               S.add_edge_e g (Node.State state_v, -1, Node.Args args_v);
               None)
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
           S.states_of_cost graph c
           |> List.iter ~f:(fun (a : State_node.t) ->
                  S.states_of_cost graph c'
                  |> List.iter ~f:(fun (a' : State_node.t) ->
                         List.iter [ Op.Union; Inter; Sub ] ~f:(fun op ->
                             let state =
                               match op with
                               | Op.Union -> Abs.union a.state a'.state
                               | Inter -> Abs.inter a.state a'.state
                               | Sub -> Abs.sub a.state a'.state
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

let reachable graph n =
  let module A = Reachable (G) in
  A.analyze ([%equal: Node.t] n) graph.Search_state.graph

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

module Dump
    (G : Graph.Graphviz.GRAPH
           with type t = G.t
            and type V.t = G.V.t
            and type E.t = G.E.t) =
struct
  let make_output_graph ?(refinement = fun _ -> false) cone separator output =
    let module Viz =
      Graph.Graphviz.Dot
        (G)
        (struct
          let graph_attributes _ = []

          let default_vertex_attributes _ = []

          let vertex_name n = Fmt.str "%d" @@ Node.id n

          let vertex_attributes n =
            let attrs = if cone n then [ `Style `Filled ] else [] in
            let attrs =
              if separator n then `Style `Dotted :: attrs else attrs
            in
            let attrs' =
              match n with
              | Node.State n ->
                  [ `HtmlLabel (Fmt.str "%a" State_node.graphviz_pp n) ]
                  @
                  if
                    Option.map output ~f:(Abs.contains n.state)
                    |> Option.value ~default:false
                  then [ `Style `Bold ]
                  else []
              | Args n ->
                  [
                    `HtmlLabel (Fmt.str "%a" Args_node.graphviz_pp n);
                    `Shape `Box;
                  ]
            in
            attrs @ attrs'

          let get_subgraph _ = None

          let default_edge_attributes _ = []

          let edge_attributes ((_, i, _) as e) =
            let attrs = if i >= 0 then [ `Label (sprintf "%d" i) ] else [] in
            let attrs =
              if refinement e then `Style `Dotted :: attrs else attrs
            in
            attrs
        end)
    in
    Viz.output_graph

  let step = ref 0

  let dump_detailed ?suffix ?output ?(cone = fun _ -> false)
      ?(separator = fun _ -> false) ?(refinement = fun _ -> false) graph =
    if !enable_dump then (
      let output_graph = make_output_graph ~refinement cone separator output in
      let fn =
        let suffix =
          Option.map suffix ~f:(sprintf "-%s") |> Option.value ~default:""
        in
        sprintf "%04d-graph%s.dot" !step suffix
      in
      Out_channel.with_file fn ~f:(fun ch ->
          output_graph ch graph.Search_state.graph);
      incr step )
end

open Dump (G)

let separators graph target =
  Seq.unfold ~init:(S.succ graph target) ~f:(fun sep ->
      if List.is_empty sep then None
      else
        let sep' =
          List.concat_map sep ~f:(S.succ graph)
          |> List.concat_map ~f:(S.succ graph)
        in
        Some (sep, sep'))

module Refinement = struct
  type t =
    [ `Add_child of Args_node.t * State_node.t * bool Map.M(Int).t
    | `Update of State_node.t * bool Map.M(Int).t
    | `Split of Args_node.t * int * Abs.t list
    | `Remove of State_node.t list ]
    list
  [@@deriving sexp]

  let pp fmt x = Sexp.pp_hum fmt @@ [%sexp_of: t] x
end

let in_cone graph target_node separator =
  let module R = Reachable (G) in
  let sep_and_args =
    separator
    |> List.concat_map ~f:(S.succ graph)
    |> List.append separator
    |> Set.of_list (module Node)
  in
  let is_separator = Set.mem sep_and_args in
  let target_reaches =
    R.analyze
      (fun v -> [%equal: Node.t] v (State target_node))
      graph.Search_state.graph
  and separator_reaches = R.analyze is_separator graph.Search_state.graph in
  fun v ->
    try
      if S.V.mem graph v then
        target_reaches v && (is_separator v || not (separator_reaches v))
      else false
    with Not_found ->
      Error.create "Node not in graph" v [%sexp_of: Node.t] |> Error.raise

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
        let succ = S.succ_e graph v in
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

let get_refinement vectors graph target_node expected_output separator =
  (* Select the subset of the graph that can reach the target. *)
  let graph =
    Search_state.
      {
        graph = cone graph (State target_node) separator;
        cost_table = [||];
        args_table = Hashtbl.create (module Args_table_key);
        state_table = Hashtbl.create (module State_table_key);
      }
  in
  dump_detailed ~suffix:"before-refine"
    ~separator:(List.mem separator ~equal:[%equal: Node.t])
    graph;

  let open Smt.Let_syntax in
  let make_vars =
    let%bind edge_var_rel =
      S.E.to_list graph
      |> List.map ~f:(fun e ->
             let%bind decl = Smt.fresh_decl ~prefix:"e" () in
             return (e, decl))
      |> Smt.all
    in

    let edge_vars = edge_var_rel |> Map.of_alist_exn (module S.E)
    and var_edges =
      List.map ~f:Tuple.T2.swap edge_var_rel |> Map.of_alist_exn (module Sexp)
    in

    let%bind state_vars =
      S.V.filter_map graph ~f:Node.to_state
      |> List.map ~f:(fun (v : State_node.t) ->
             let%bind vars =
               List.map vectors ~f:(fun vec ->
                   match Map.find v.state vec with
                   | Some x -> return @@ Smt.Bool.bool x
                   | None ->
                       Smt.fresh_decl ~prefix:(sprintf "s%d_b%d_" v.id vec) ())
               |> Smt.all
             in
             return (v, vars))
      |> Smt.all
    in
    let state_vars = Map.of_alist_exn (module State_node) state_vars in

    let%bind arg_out_vars =
      S.V.filter_map graph ~f:Node.to_args
      |> List.map ~f:(fun (v : Args_node.t) ->
             let%bind vars =
               List.init (List.length vectors) ~f:(fun b ->
                   Smt.fresh_decl ~prefix:(sprintf "a%d_b%d_" v.id b) ())
               |> Smt.all
             in
             return (v, vars))
      |> Smt.all
    in
    let arg_out_vars = Map.of_alist_exn (module Args_node) arg_out_vars in
    return (edge_vars, var_edges, state_vars, arg_out_vars)
  in

  let output_constr state_vars =
    (* The output state must have the expected value. *)
    List.map2_exn (Map.find_exn state_vars target_node)
      (Array.to_list expected_output) ~f:(fun x v -> Smt.Bool.(x = bool v))
    |> Smt.Bool.and_
    |> Smt.make_defn "correct-output"
    >>= Smt.Interpolant.assert_group
  in

  let synth_constrs ~assert_group
      (edge_vars, var_edges, state_vars, arg_out_vars) =
    (* Every selected state node must select exactly one dependent args node. *)
    let%bind () =
      S.V.filter_map graph ~f:Node.to_state
      |> List.map ~f:(fun v ->
             let selected =
               if [%equal: State_node.t] target_node v then
                 [ Smt.Bool.(bool true) ]
               else
                 S.pred_e graph (State v)
                 |> List.map ~f:(Map.find_exn edge_vars)
             in
             let deps =
               S.succ_e graph (State v) |> List.map ~f:(Map.find_exn edge_vars)
             in
             if not (List.is_empty deps) then
               Smt.(
                 make_defn
                   (sprintf "state-%d-deps" v.id)
                   Bool.(or_ selected => exactly_one deps)
                 >>= assert_group `A)
             else return ())
      |> Smt.all_unit
    in

    (* An arg node with a selected outgoing edge must select all its incoming
       edges. *)
    let%bind () =
      S.V.filter_map graph ~f:Node.to_args
      |> List.map ~f:(fun v ->
             let outgoing =
               S.pred_e graph (Args v) |> List.map ~f:(Map.find_exn edge_vars)
             in
             let incoming =
               S.succ_e graph (Args v) |> List.map ~f:(Map.find_exn edge_vars)
             in
             Smt.Bool.(or_ outgoing => and_ incoming))
      |> Smt.Bool.and_
      |> Smt.make_defn "args-have-all-incoming"
      >>= assert_group `A
    in

    let%bind () =
      S.V.filter_map graph ~f:Node.to_state
      |> List.map ~f:(fun v ->
             let state_in = Map.find_exn state_vars v in
             S.succ_e graph (State v)
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
      >>= assert_group `A
    in

    let%bind () =
      S.V.filter_map graph ~f:Node.to_args
      |> List.map ~f:(fun v ->
             let incoming_edges = S.succ_e graph (Args v) in

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
               match (v.op, incoming_states) with
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
                     [%sexp_of: Op.t * Sexp.t list list]
                   |> Error.raise
             in

             let%bind defn =
               Smt.make_defn
                 (Fmt.str "semantics-%a-%d" Op.pp v.op v.id)
                 (Smt.Bool.and_ semantic)
             in
             if List.mem separator (Args v) ~equal:[%equal: Node.t] then
               assert_group `B defn
             else assert_group `A defn)
      |> Smt.all_unit
    in
    return ()
  in

  let rec interpolant_vars =
    let open Or_error.Let_syntax in
    function
    | Sexp.List (Atom ("and" | "not" | "or") :: args) ->
        let%map args = List.map ~f:interpolant_vars args |> Or_error.all in
        Set.union_list (module String) args
    | List [ Atom "let"; List [ List [ Atom var; lhs ] ]; rhs ] ->
        let%bind lhs = interpolant_vars lhs in
        let%bind rhs = interpolant_vars rhs in
        let rhs = Set.remove rhs var in
        return @@ Set.union lhs rhs
    | Atom x -> return @@ Set.singleton (module String) x
    | inter -> Or_error.error "Unexpected interpolant" inter [%sexp_of: Sexp.t]
  in

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
  in

  let refinement_of_model forced state_vars arg_out_vars ivars =
    let forced = Map.of_alist_exn (module String) forced in

    let var_names =
      Map.map ~f:(List.map ~f:(function Sexp.Atom x -> x | _ -> assert false))
    in
    let arg_out_vars = arg_out_vars |> var_names in
    let state_vars = state_vars |> var_names in
    let open Option.Let_syntax in
    let refinement =
      separator
      |> List.map ~f:Node.to_args_exn
      |> List.dedup_and_sort ~compare:[%compare: Args_node.t]
      (* Only care about args nodes with refined output bits. *)
      |> List.filter_map ~f:(fun v ->
             let get_bits =
               List.filter_mapi ~f:(fun i x ->
                   if Set.mem ivars x then Some (i, Map.find_exn forced x)
                   else None)
             in
             let bits = Map.find_exn arg_out_vars v |> get_bits in
             let bits' =
               S.pred graph (Args v)
               |> List.map ~f:Node.to_state_exn
               |> List.map ~f:(Map.find_exn state_vars)
               |> List.map ~f:get_bits |> List.concat
             in
             let refined_bits = normalize_bits (bits @ bits') in
             if List.is_empty refined_bits then None else Some (v, refined_bits))
      |> List.concat_map ~f:(fun (v, refined_bits) ->
             let (state_nodes : State_node.t list) =
               S.pred graph (Args v) |> List.map ~f:Node.to_state_exn
             in

             Fmt.epr "Refined bits: %a@."
               Fmt.(Dump.(list @@ pair int @@ option bool))
               refined_bits;

             let states = List.map state_nodes ~f:State_node.state in
             let refined =
               List.fold refined_bits ~init:states
                 ~f:(fun states (bit, forced) ->
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
                 let cost = List.hd_exn state_nodes |> State_node.cost in
                 [ `Split (v, cost, refined) ]
               else []
             in
             ret)
    in

    let edges =
      List.concat_map refinement ~f:(function
        | `Split (v, _, _) -> S.pred_e graph (Args v)
        | `Remove vs ->
            List.concat_map ~f:(fun v -> S.succ_e graph (State v)) vs)
      |> Set.of_list (module S.E)
    in
    dump_detailed ~suffix:"after-refine"
      ~separator:(List.mem separator ~equal:[%equal: Node.t])
      ~refinement:(Set.mem edges) graph;

    Fmt.epr "Refinement: %a@." Refinement.pp refinement;
    if List.is_empty refinement then failwith "No-op refinement" else refinement
  in

  let parse_model sexp =
    let error s =
      Error.create "Unexpected model" s [%sexp_of: Sexp.t] |> Error.raise
    in
    let parse_value = function
      | Sexp.Atom "true" -> true
      | Atom "false" -> false
      | s -> error s
    in
    match sexp with
    | Sexp.List vals ->
        List.map vals ~f:(fun v ->
            match v with
            | List [ Atom name; value ] -> (name, parse_value value)
            | s -> error s)
    | s -> error s
  in

  let forced_bits interpolant =
    interpolant_vars interpolant
    |> Or_error.ok_exn |> Set.to_list
    |> List.map ~f:(fun v ->
           let vv = Sexp.Atom v in
           let open Smt.Let_syntax in
           let check_true =
             let%bind _ = make_vars in
             let%bind () = Smt.(assert_ Bool.(not (interpolant => vv))) in
             Smt.check_sat
           in
           let check_false =
             let%bind _ = make_vars in
             let%bind () = Smt.(assert_ Bool.(not (interpolant => not vv))) in
             Smt.check_sat
           in
           if not (Smt.run check_true) then (v, Some true)
           else if not (Smt.run check_false) then (v, Some false)
           else (v, None))
  in

  let process_interpolant state_vars arg_out_vars interpolant =
    Fmt.epr "Interpolant: %a@." Sexp.pp_hum interpolant;
    refinement_of_model (forced_bits interpolant) state_vars arg_out_vars
      (interpolant_vars interpolant |> Or_error.ok_exn)
  in

  let process_model var_edges sexp =
    let model = parse_model sexp in
    List.filter_map model ~f:(fun (e, is_selected) ->
        if is_selected then Map.find var_edges (Sexp.Atom e) else None)
    |> Set.of_list (module S.E)
  in

  let get_interpolant =
    let%bind ((edge_vars, var_edges, state_vars, arg_out_vars) as vars) =
      make_vars
    in
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

  let arg_out_vars, state_vars, var_edges, ret = Smt.run get_interpolant in
  Either.map
    ~first:(process_interpolant state_vars arg_out_vars)
    ~second:(process_model var_edges) ret

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
  S.filter graph ~f:(fun v -> not (should_prune v));
  let size' = G.nb_vertex graph.Search_state.graph in
  Fmt.epr "Pruning: size before=%d, after=%d, removed %f%%\n" size size'
    Float.(100.0 - (of_int size' / of_int size * 100.0))

let fix_up_args graph =
  let to_remove =
    S.V.filter graph ~f:(function
      | Args a as v ->
          let succ = S.succ graph v in
          List.length succ <> Op.arity a.op
      | State _ -> false)
  in
  if List.is_empty to_remove then false
  else (
    S.remove_vertexes graph to_remove;
    true )

let fix_up_states graph =
  let to_remove =
    S.V.filter graph ~f:(function
      | Args _ -> false
      | State _ as v ->
          let succ = S.succ graph v in
          List.is_empty succ)
  in
  if List.is_empty to_remove then false
  else (
    S.remove_vertexes graph to_remove;
    true )

let prune graph separator refinement =
  let size = G.nb_vertex graph.Search_state.graph in

  List.iter refinement ~f:(function
    | `Split (v, _, _) ->
        S.pred_e graph (Args v)
        |> List.iter ~f:(G.remove_edge_e graph.Search_state.graph)
    | `Remove vs -> ());

  let rec loop () = if fix_up_args graph || fix_up_states graph then loop () in
  loop ();

  let size' = G.nb_vertex graph.Search_state.graph in
  Fmt.epr "Pruning: size before=%d, after=%d, removed %f%%\n" size size'
    Float.(100.0 - (of_int size' / of_int size * 100.0))

let arg_cost graph arg =
  S.succ graph (Args arg)
  |> List.filter_map ~f:Node.to_state
  |> List.sum (module Int) ~f:(fun v -> v.State_node.cost)
  |> fun cost -> cost + 1

let refine_level n graph =
  S.V.fold graph ~init:(0, 0) ~f:(fun ((num, dem) as acc) -> function
    | Args _ -> acc | State v -> (num + Abs.width v.state, dem + n))

let refine graph output separator refinement =
  dump_detailed ~output ~suffix:"before-pruning" graph;

  prune graph separator refinement;

  dump_detailed ~output ~suffix:"after-pruning" graph;

  List.iter refinement ~f:(function
    | `Split (v, cost, refined) ->
        if S.V.mem graph (Args v) then
          List.iter refined ~f:(fun state ->
              let (`Fresh v' | `Stale v') =
                State_node.create_consed ~state ~cost graph
              in
              S.add_edge_e graph (State v', -1, Args v))
    | `Remove vs ->
        S.remove_vertexes graph @@ List.map ~f:(fun v -> Node.State v) vs);

  dump_detailed ~output ~suffix:"after-refinement" graph;

  let num, dem = refine_level (Array.length output) graph in
  Fmt.epr "Refine level: %d/%d (%f%%)\n" num dem
    Float.(of_int num / of_int dem * 100.0)

let rec extract_program graph selected_edges target =
  let args =
    S.succ_e graph target
    |> List.filter ~f:(Set.mem selected_edges)
    |> List.map ~f:(fun (_, _, v) -> v)
    |> List.map ~f:Node.to_args_exn
  in
  match args with
  | [ a ] ->
      `Apply
        ( a.Args_node.op,
          S.succ graph (Args a)
          |> List.map ~f:(extract_program graph selected_edges) )
  | args ->
      Error.create "Too many args" args [%sexp_of: Args_node.t list]
      |> Error.raise

let synth ?(no_abstraction = false) inputs output =
  let graph = S.create !max_cost in
  let vectors = List.init (Array.length @@ List.hd_exn inputs) ~f:Fun.id in

  let refute () =
    match
      S.V.find_map graph ~f:(function
        | State v when Abs.contains v.state output -> Some v
        | _ -> None)
    with
    | Some target ->
        let seps = separators graph (State target) |> Seq.to_list in
        let seps, last_sep =
          match List.rev seps with
          | last :: rest -> (List.rev rest, last)
          | _ -> failwith "No separators"
        in

        let refinement =
          List.find_map seps ~f:(fun sep ->
              let open Option.Let_syntax in
              let%map r =
                get_refinement vectors graph target output sep
                |> Either.First.to_option
              in
              (sep, r))
        in
        ( match refinement with
        | Some (sep, r) -> refine graph output sep r
        | None -> (
            match get_refinement vectors graph target output last_sep with
            | First r -> refine graph output last_sep r
            | Second selected_edges ->
                Fmt.epr "Could not refute: %a" Sexp.pp_hum
                  ( [%sexp_of: Program.t]
                  @@ extract_program graph selected_edges (State target) );
                raise (Done `Sat) ) );
        true
    | None -> false
  in

  let rec loop cost =
    if cost > !max_cost then raise (Done `Unsat);
    let rec strengthen_and_cover () =
      let did_strengthen = refute () in
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
      S.V.filter_map graph ~f:(function
        | State v -> Some (Abs.width v.state)
        | _ -> None)
      |> List.sort ~compare:[%compare: int]
      |> Array.of_list
    in
    ( graph,
      Stats.
        {
          n_state_nodes =
            S.V.filter graph ~f:(function State v -> true | _ -> false)
            |> List.length;
          n_arg_nodes =
            S.V.filter graph ~f:(function Args v -> true | _ -> false)
            |> List.length;
          n_covered = -1;
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
    if Program.size p > !max_cost then sample_prog () else p
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
        S.V.find_map graph ~f:(function
          | State v when Abs.contains v.state cstate -> Some v
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
