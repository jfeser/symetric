open Ast

module Is_fresh = struct
  type 'a t = Fresh of 'a | Stale of 'a

  let is_fresh = function Fresh _ -> true | _ -> false

  let unwrap (Fresh x | Stale x) = x
end

open Is_fresh

module Option_vector : sig
  type 'a t

  val create : int -> 'a t

  val reserve : 'a t -> int -> unit

  val get_some_exn : 'a t -> int -> 'a

  val set_some : 'a t -> int -> 'a -> unit

  val get : 'a t -> int -> 'a option
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

  let get a = Option_array.get !a
end

module State = struct
  let costs = Option_vector.create 128

  let states = Option_vector.create 128

  let types = Option_vector.create 128

  let state_cost_idx =
    lazy (Array.init !Global.max_cost ~f:(fun _ -> Hashtbl.create (module Abs)))

  let id_ctr = ref 1

  let state id = Option_vector.get_some_exn states (id / 2)

  let cost id = Option_vector.get_some_exn costs (id / 2)

  let type_ id = Option_vector.get_some_exn types (id / 2)

  module T = struct
    type t = int [@@deriving compare, equal, hash]

    let sexp_of_t id = [%sexp_of: int * Abs.t * int] (id, state id, cost id)
  end

  include T
  include Comparator.Make (T)

  let get s c =
    let idx = c - 1 in
    [%test_pred: int] ~message:"index out of bounds"
      (fun idx -> idx >= 0 && idx < Array.length (Lazy.force state_cost_idx))
      idx;
    let state_idx = (Lazy.force state_cost_idx).(c - 1) in
    Hashtbl.find state_idx s

  let set s c id =
    Hashtbl.set ~key:s ~data:id (Lazy.force state_cost_idx).(c - 1)

  let of_cost c = Hashtbl.data (Lazy.force state_cost_idx).(c - 1)

  let create s c t =
    match get s c with
    | Some id -> Stale id
    | None ->
        let id = !id_ctr in
        id_ctr := !id_ctr + 2;
        let idx = id / 2 in
        Option_vector.reserve states idx;
        Option_vector.reserve costs idx;
        Option_vector.reserve types idx;
        Option_vector.set_some states idx s;
        Option_vector.set_some costs idx c;
        Option_vector.set_some types idx t;
        set s c id;
        Fresh id

  let id = ident

  let graphviz_pp fmt id =
    Fmt.pf fmt "%a<br/>id=%d cost=%d" Abs.graphviz_pp (state id) id (cost id)
end

module Hyper_edge = struct
  module T = struct
    type t = State.t list * Op.t * State.t [@@deriving compare, hash, sexp_of]
  end

  include T

  let hyper_edges = Hash_set.create (module T)

  let mem = Hash_set.mem hyper_edges

  let add = Hash_set.add hyper_edges

  let remove = Hash_set.remove hyper_edges
end

module Args = struct
  let ops = Option_vector.create 128

  let hyper_edges = Option_vector.create 128

  let id_ctr = ref 0

  let id = ident

  let op id = Option_vector.get_some_exn ops (id / 2)

  let hyper_edge id = Option_vector.get hyper_edges (id / 2)

  let set_hyper_edge id = Option_vector.set_some hyper_edges (id / 2)

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
    Option_vector.reserve hyper_edges idx;
    id
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

  let type_ =
    match_ ~args:(fun arg_v -> Args.op arg_v |> Op.ret_type) ~state:State.type_
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

type t = G.t

let create () = G.create ()

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

module Pred = struct
  include Container.Make0 (struct
    type nonrec t = G.t * G.V.t

    module Elt = struct
      type t = G.V.t [@@deriving equal]
    end

    let fold (g, v) ~init ~f = G.fold_pred (fun v acc -> f acc v) g v init

    let iter = `Custom (fun (g, v) ~f -> G.iter_pred f g v)

    let length = `Custom (fun (g, v) -> G.in_degree g v)
  end)
end

module Succ = struct
  include Container.Make0 (struct
    type nonrec t = G.t * G.V.t

    module Elt = struct
      type t = G.V.t [@@deriving equal]
    end

    let fold (g, v) ~init ~f = G.fold_succ (fun v acc -> f acc v) g v init

    let iter = `Custom (fun (g, v) ~f -> G.iter_succ f g v)

    let length = `Custom (fun (g, v) -> G.out_degree g v)
  end)
end

let filter g ~f =
  V.filter g ~f:(Fun.negate f)
  |> List.iter ~f:(fun v -> if G.mem_vertex g v then G.remove_vertex g v)

let nb_vertex = G.nb_vertex

let states_of_cost g cost =
  State.of_cost cost |> List.filter ~f:(G.mem_vertex g)

let inputs g arg_v =
  G.succ_e g (Node.of_args arg_v)
  |> List.map ~f:(fun (_, n, v) -> (Node.to_state_exn v, n))
  |> List.sort ~compare:(fun (_, n) (_, n') -> [%compare: int] n n')
  |> List.map ~f:(fun (v, _) -> v)

let remove_args g v =
  Option.iter (Args.hyper_edge v) ~f:Hyper_edge.remove;
  G.remove_vertex g (Node.of_args v)

let fix_up_args work add_work state args_v =
  let v = Node.of_args args_v in
  if G.out_degree state v <> Op.arity (Args.op args_v) then (
    let work' = G.fold_pred (fun v' w -> add_work w v') state v work in
    remove_args state args_v;
    work' )
  else if G.in_degree state v = 0 then (
    remove_args state args_v;
    work )
  else work

let fix_up_states work add_work state state_v =
  let v = Node.of_state state_v in
  if G.out_degree state v = 0 then (
    let work' = G.fold_pred (fun v' w -> add_work w v') state v work in
    G.remove_vertex state v;
    work' )
  else work

let fix_up state =
  let worklist = Hash_queue.create @@ Hashtbl.Hashable.of_key (module Node) in
  let add_work () v = Hash_queue.enqueue_back worklist v v |> ignore in
  V.iter state ~f:(add_work ());
  let fix_node v =
    Node.match_
      ~args:(fix_up_args () add_work state)
      ~state:(fix_up_states () add_work state)
      v
  in
  let rec loop () =
    match Hash_queue.dequeue_back worklist with
    | Some v ->
        fix_node v;
        loop ()
    | None -> ()
  in
  loop ()

let insert_hyper_edge graph state_v_ins op state_v_out =
  let args_v = Args.create op |> Node.of_args in
  let hyper_edge = (state_v_ins, op, state_v_out) in
  Args.set_hyper_edge args_v hyper_edge;
  Hyper_edge.add hyper_edge;
  List.iteri state_v_ins ~f:(fun i v ->
      G.add_edge_e graph (args_v, i, Node.of_state v));
  G.add_edge_e graph (Node.of_state state_v_out, -1, args_v)

let insert_hyper_edge_if_not_exists graph state_v_ins op state_v_out =
  let hyper_edge = (state_v_ins, op, state_v_out) in
  if not (Hyper_edge.mem hyper_edge) then
    insert_hyper_edge graph state_v_ins op state_v_out
