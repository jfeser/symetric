module Is_fresh = struct
  type 'a t = Fresh of 'a | Stale of 'a
end

open Is_fresh

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

  let create n = ref (Option_array.create ~len:n)

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
    type t = int [@@deriving compare, equal, hash]

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

  let state_cost_idx =
    Array.init !Global.max_cost ~f:(fun _ -> Hashtbl.create (module Abs))

  let id_ctr = ref 1

  let state id = Option_vector.get_some_exn states (id / 2)

  let cost id = Option_vector.get_some_exn costs (id / 2)

  module T = struct
    type t = int [@@deriving compare, equal, hash]

    let sexp_of_t id = [%sexp_of: int * Abs.t * int] (id, state id, cost id)
  end

  include T
  include Comparator.Make (T)

  let get s c =
    let state_idx = state_cost_idx.(c - 1) in
    Hashtbl.find state_idx s

  let set s c id = Hashtbl.set ~key:s ~data:id state_cost_idx.(c - 1)

  let of_cost c = Hashtbl.data state_cost_idx.(c - 1)

  let create s c =
    match get s c with
    | Some id -> Stale id
    | None ->
        let id = !id_ctr in
        id_ctr := !id_ctr + 2;
        let idx = id / 2 in
        Option_vector.reserve states idx;
        Option_vector.reserve costs idx;
        Option_vector.set_some states idx s;
        Option_vector.set_some costs idx c;
        set s c id;
        Fresh id

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
  include Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node) (Edge)

  let changed = ref false

  let add_vertex g v =
    changed := !changed || not (mem_vertex g v);
    add_vertex g v

  let remove_vertex g v =
    changed := !changed || mem_vertex g v;
    remove_vertex g v

  let add_edge g v v' =
    changed := !changed || not (mem_edge g v v');
    add_edge g v v'

  let add_edge_e g e =
    changed := !changed || not (mem_edge_e g e);
    add_edge_e g e

  let remove_edge g v v' =
    changed := !changed || mem_edge g v v';
    remove_edge g v v'

  let remove_edge_e g e =
    changed := !changed || mem_edge_e g e;
    remove_edge_e g e

  let has_changed () = !changed

  let reset_changed () = changed := false

  let iter_succ_e f g v =
    try iter_succ_e f g v
    with Invalid_argument msg ->
      raise_s [%message "iter_succ_e failed" (msg : string) (v : Node.t)]
end

module Args_table_key = struct
  module T = struct
    type t = Op.t * State.t list [@@deriving compare, hash, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

type t = { graph : G.t; args_table : Args.t Hashtbl.M(Args_table_key).t }

let create () =
  { graph = G.create (); args_table = Hashtbl.create (module Args_table_key) }

module V = struct
  include Comparator.Make (struct
    type t = Node.t [@@deriving compare, sexp_of]
  end)

  include Container.Make0 (struct
    type nonrec t = G.t

    module Elt = struct
      type t = G.V.t [@@deriving equal]
    end

    let fold g ~init ~f = G.fold_vertex (fun v acc -> f acc v) g init

    let iter = `Custom (fun g ~f -> G.iter_vertex f g)

    let length = `Custom G.nb_vertex
  end)

  let filter g ~f = fold ~f:(fun xs x -> if f x then x :: xs else xs) g ~init:[]

  let filter_map g ~f =
    fold
      ~f:(fun xs x -> match f x with Some x' -> x' :: xs | None -> xs)
      g ~init:[]

  include G.V
end

module E = struct
  include Comparator.Make (struct
    type t = Node.t * int * Node.t [@@deriving compare, sexp_of]
  end)

  include Container.Make0 (struct
    type nonrec t = G.t

    module Elt = struct
      type t = G.E.t

      let equal e e' = G.E.compare e e' = 0
    end

    let fold g ~init ~f = G.fold_edges_e (fun v acc -> f acc v) g init

    let iter = `Custom (fun g ~f -> G.iter_edges_e f g)

    let length = `Custom G.nb_edges
  end)

  include G.E
end

let remove_vertexes g vs =
  let vs = List.filter vs ~f:(G.mem_vertex g.graph) in
  let to_remove = Set.of_list (module V) vs in
  let should_keep v = not (Set.mem to_remove v) in

  Set.iter to_remove ~f:(G.remove_vertex g.graph);
  Hashtbl.filteri_inplace g.args_table ~f:(fun ~key:(_, states) ~data ->
      should_keep data
      && List.for_all states ~f:(fun v -> should_keep @@ Node.of_state v))

let filter g ~f = remove_vertexes g @@ V.filter g.graph ~f:(Fun.negate f)

let nb_vertex g = G.nb_vertex g.graph

let states_of_cost g cost =
  State.of_cost cost |> List.filter ~f:(G.mem_vertex g.graph)

let check g =
  Hashtbl.iter g.args_table ~f:(fun v ->
      assert (G.mem_vertex g.graph @@ Node.of_args v))

let inputs g arg_v =
  G.succ_e g.graph (Node.of_args arg_v)
  |> List.map ~f:(fun (_, n, v) -> (Node.to_state_exn v, n))
  |> List.sort ~compare:(fun (_, n) (_, n') -> [%compare: int] n n')
  |> List.map ~f:(fun (v, _) -> v)

let fix_up_args work add_work state args_v =
  let v = Node.of_args args_v in
  if G.out_degree state.graph v <> Op.arity (Args.op args_v) then (
    let work' = G.fold_pred (fun v' w -> add_work w v') state.graph v work in
    remove_vertexes state [ v ];
    work' )
  else if G.in_degree state.graph v = 0 then (
    remove_vertexes state [ v ];
    work )
  else work

let fix_up_states work add_work state state_v =
  let v = Node.of_state state_v in
  if G.out_degree state.graph v = 0 then (
    let work' = G.fold_pred (fun v' w -> add_work w v') state.graph v work in
    remove_vertexes state [ v ];
    work' )
  else work

let fix_up state =
  let worklist = Queue.of_list @@ V.to_list state.graph in
  let add_work () v = Queue.enqueue worklist v in
  let fix_node v =
    if V.mem state.graph v then
      Node.match_
        ~args:(fix_up_args () add_work state)
        ~state:(fix_up_states () add_work state)
        v
  in
  let rec loop () =
    match Queue.dequeue worklist with
    | Some v ->
        fix_node v;
        loop ()
    | None -> ()
  in
  loop ()
