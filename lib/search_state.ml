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

module Option_vector : sig
  type 'a t

  val create : int -> 'a t

  val reserve : 'a t -> int -> unit

  val get_some_exn : 'a t -> int -> 'a

  val set_some : 'a t -> int -> 'a -> unit
end = struct
  type 'a t = 'a Option_array.t ref

  let create n = ref (Option_array.create n)

  let reserve a n =
    let new_len = Int.ceil_pow2 (n + 1) and old_len = Option_array.length !a in
    if old_len < new_len then (
      let a' = Option_array.create ~len:new_len in
      Option_array.blit ~src:!a ~src_pos:0 ~len:old_len ~dst:a' ~dst_pos:0;
      a := a' )

  let get_some_exn a = Option_array.get_some_exn !a

  let set_some a = Option_array.set_some !a
end

module Args = struct
  let ops = Option_vector.create 128

  let id_ctr = ref 0

  let id = ident

  let op id = Option_vector.get_some_exn ops (id / 2)

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
    let idx = id / 2 in
    Option_vector.reserve ops idx;
    Option_vector.set_some ops idx op;
    id
end

module State = struct
  let costs = Option_vector.create 128

  let states = Option_vector.create 128

  let id_ctr = ref 1

  let state id = Option_vector.get_some_exn states (id / 2)

  let cost id = Option_vector.get_some_exn costs (id / 2)

  module T = struct
    type t = int [@@deriving compare, equal, hash, show]

    let sexp_of_t id = [%sexp_of: int * Abs.t * int] (id, state id, cost id)
  end

  include T
  include Comparator.Make (T)

  let create s c =
    let id = !id_ctr in
    id_ctr := !id_ctr + 2;
    let idx = id / 2 in
    Option_vector.reserve states idx;
    Option_vector.reserve costs idx;
    Option_vector.set_some states idx s;
    Option_vector.set_some costs idx c;
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
  let should_keep v = not (Set.mem to_remove v) in

  for i = 0 to Array.length g.cost_table - 1 do
    g.cost_table.(i) <- List.filter g.cost_table.(i) ~f:should_keep
  done;

  Set.iter to_remove ~f:(G.remove_vertex g.graph);
  Hashtbl.filteri_inplace g.args_table ~f:(fun ~key:(_, states) ~data ->
      should_keep data
      && List.for_all states ~f:(fun v -> should_keep @@ Node.of_state v));
  Hashtbl.filter_inplace g.state_table ~f:should_keep

let remove_vertex x v = remove_vertexes x [ v ]

let filter g ~f = remove_vertexes g @@ V.filter g ~f:(Fun.negate f)

let nb_vertex g = G.nb_vertex g.graph

let states_of_cost g cost =
  V.filter_map g ~f:Node.to_state
  |> List.filter ~f:(fun v -> State.cost v = cost)

(* let idx = cost - 1 in
 * let ret = g.cost_table.(idx) in
 * [%test_pred: State.t list]
 *   (List.for_all ~f:(fun v -> V.mem g @@ Node.of_state v))
 *   ret;
 * ret *)

let set_states_of_cost g cost states =
  [%test_pred: State.t list]
    (List.for_all ~f:(fun s -> State.cost s = cost))
    states;
  let idx = cost - 1 in
  g.cost_table.(idx) <- states

let check g =
  Hashtbl.iter g.args_table ~f:(fun v ->
      assert (G.mem_vertex g.graph @@ Node.of_args v));
  Hashtbl.iter g.state_table ~f:(fun v ->
      assert (G.mem_vertex g.graph @@ Node.of_state v));
  Array.iter g.cost_table
    ~f:
      (List.iter ~f:(fun v -> assert (G.mem_vertex g.graph @@ Node.of_state v)))

let inputs g arg_v =
  succ_e g (Node.of_args arg_v)
  |> List.map ~f:(fun (_, n, v) -> (Node.to_state_exn v, n))
  |> List.sort ~compare:(fun (_, n) (_, n') -> [%compare: int] n n')
  |> List.map ~f:(fun (v, _) -> v)
