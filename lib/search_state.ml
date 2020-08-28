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

module Args = struct
  let ops = Option_array.create 1_000_000

  let id_ctr = ref 0

  let id = ident

  let op id = Option_array.get_some_exn ops (id / 2)

  module T = struct
    type t = int [@@deriving compare, equal, hash, show]

    let sexp_of_t id = [%sexp_of: int * Op.t] (id, op id)
  end

  include T
  include Comparator.Make (T)

  let graphviz_pp fmt x = Fmt.pf fmt "%a<br/>id=%d" Op.pp (op x) (id x)

  let create op =
    let id = !id_ctr in
    id_ctr := !id_ctr + 2;
    Option_array.set_some ops (id / 2) op;
    id
end

module State = struct
  let costs = Option_array.create 1_000_000

  let states = Option_array.create 1_000_000

  let id_ctr = ref 1

  let state id = Option_array.get_some_exn states (id / 2)

  let cost id = Option_array.get_some_exn costs (id / 2)

  module T = struct
    type t = int [@@deriving compare, equal, hash, show]

    let sexp_of_t id = [%sexp_of: int * Abs.t * int] (id, state id, cost id)
  end

  include T
  include Comparator.Make (T)

  let create s c =
    let id = !id_ctr in
    id_ctr := !id_ctr + 2;
    Option_array.set_some states (id / 2) s;
    Option_array.set_some costs (id / 2) c;
    id

  let id = ident

  let graphviz_pp fmt id =
    Fmt.pf fmt "%a<br/>id=%d cost=%d" Abs.graphviz_pp (state id) id (cost id)
end

module Node = struct
  let is_args v = v mod 2 = 0

  let is_state v = not (is_args v)

  let match_ ~args ~state v = if is_args v then args v else state v

  module T = struct
    type t = int [@@deriving compare, equal, hash, show]

    let sexp_of_t v =
      match_ ~args:[%sexp_of: Args.t] ~state:[%sexp_of: State.t] v
  end

  include T
  include Comparator.Make (T)

  let id = ident

  let of_args = ident

  let of_state = ident

  let to_args v = match_ ~args:(fun x -> Some x) ~state:(fun _ -> None) v

  let to_args_exn x = Option.value_exn (to_args x)

  let to_state v = match_ ~args:(fun _ -> None) ~state:(fun x -> Some x) v

  let to_args_exn v =
    match_ ~args:ident
      ~state:(fun x ->
        Error.create "Expected args" x [%sexp_of: State.t] |> Error.raise)
      v

  let to_state_exn v =
    match_
      ~args:(fun x ->
        Error.create "Expected state" x [%sexp_of: Args.t] |> Error.raise)
      ~state:ident v
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

    let vertex_attributes v =
      Node.match_
        ~args:(fun _ -> [ `Shape `Point ])
        ~state:(fun x -> [ `HtmlLabel (Fmt.str "%a" State.pp x) ])
        v

    let get_subgraph _ = None

    let default_edge_attributes _ = []

    let edge_attributes (_, i, _) = [ `Label (sprintf "%d" i) ]
  end

  include G
  include Graph.Graphviz.Dot (G) (Attr)
end

module Args_table_key = struct
  module T = struct
    type t = Op.t * State.t list [@@deriving compare, hash, sexp_of]
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
  args_table : Args.t Hashtbl.M(Args_table_key).t;
  state_table : State.t Hashtbl.M(State_table_key).t;
  cost_table : State.t list array;
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

let remove_vertexes g vs =
  let vs = List.filter vs ~f:(G.mem_vertex g.graph) in
  let to_remove = Set.of_list (module V) vs in
  let remove v = not (Set.mem to_remove v) in

  for i = 0 to Array.length g.cost_table - 1 do
    g.cost_table.(i) <- List.filter g.cost_table.(i) ~f:remove
  done;
  Set.iter to_remove ~f:(G.remove_vertex g.graph);
  Hashtbl.filter_inplace g.args_table ~f:remove;
  Hashtbl.filter_inplace g.state_table ~f:remove

let remove_vertex x v = remove_vertexes x [ v ]

let filter g ~f = remove_vertexes g @@ V.filter g ~f:(Fun.negate f)

let states_of_cost g cost =
  let idx = cost - 1 in
  g.cost_table.(idx)

let set_states_of_cost g cost states =
  assert (List.for_all states ~f:(fun s -> State.cost s = cost));
  let idx = cost - 1 in
  g.cost_table.(idx) <- states
