let mk_id =
  let ctr = ref 0 in
  fun () ->
    incr ctr;
    !ctr

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

module Args_node0 : sig
  type t = private int [@@deriving compare, equal, hash, sexp_of, show]

  include Comparator.S with type t := t

  val create : Op.t -> t

  val id : t -> int

  val op : t -> Op.t

  val graphviz_pp : t Fmt.t
end = struct
  let ops = Option_array.create 1_000_000

  let id = ident

  let op x = Option_array.get_some_exn ops x

  module T = struct
    type t = int [@@deriving compare, equal, hash, show]

    let sexp_of_t id = [%sexp_of: int * Op.t] (id, op id)
  end

  include T
  include Comparator.Make (T)

  let graphviz_pp fmt x = Fmt.pf fmt "%a<br/>id=%d" Op.pp (op x) (id x)

  let create op =
    let id = mk_id () in
    Option_array.set_some ops id op;
    id
end

module State_node0 : sig
  type t [@@deriving compare, equal, hash, sexp, show]

  include Comparator.S with type t := t

  val create : Abs.t -> int -> t

  val id : t -> int

  val state : t -> Abs.t

  val cost : t -> int

  val graphviz_pp : t Fmt.t
end = struct
  module T = struct
    type t = { id : int; cost : int; [@ignore] state : Abs.t [@ignore] }
    [@@deriving compare, equal, hash, sexp, show]
  end

  include T
  include Comparator.Make (T)

  let create state cost =
    let id = mk_id () in
    Fmt.epr "Created %d %d\n" id cost;
    { id; state; cost }

  let id { id; _ } = id

  let state { state; _ } = state

  let cost { cost; _ } = cost

  let graphviz_pp fmt { state; cost; id; _ } =
    Fmt.pf fmt "%a<br/>id=%d cost=%d" Abs.graphviz_pp state id cost
end

module Node = struct
  module T = struct
    type t = Args of Args_node0.t | State of State_node0.t
    [@@deriving compare, equal, hash, sexp_of, show]
  end

  include T
  include Comparator.Make (T)

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
    type t = Node.t [@@deriving compare, sexp_of]
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

  let filter g ~f = fold ~f:(fun xs x -> if f x then x :: xs else xs) g ~init:[]

  let filter_map g ~f =
    fold
      ~f:(fun xs x -> match f x with Some x' -> x' :: xs | None -> xs)
      g ~init:[]

  let map g ~f = fold ~f:(fun xs x -> f x :: xs) g ~init:[]

  include G.V
end

module E = struct
  include Comparator.Make (struct
    type t = Node.t * int * Node.t [@@deriving compare, sexp_of]
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

  let filter g ~f = fold ~f:(fun xs x -> if f x then x :: xs else xs) g ~init:[]

  let map g ~f = fold ~f:(fun xs x -> f x :: xs) g ~init:[]

  include G.E
end

let succ = wrap G.succ

let pred = wrap G.pred

let succ_e = wrap G.succ_e

let pred_e = wrap G.pred_e

let add_edge_e g e = G.add_edge_e g.graph e

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
  assert (List.for_all states ~f:(fun s -> State_node0.cost s = cost));
  let idx = cost - 1 in
  g.cost_table.(idx) <- states
