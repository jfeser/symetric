open Ast

(* module ConcreteBidirectionalLabeled (V : sig
 *   type t [@@deriving compare, hash, sexp_of]
 * end) (L : sig
 *   type t [@@deriving compare, sexp_of]
 * 
 *   val default : t
 * end) =
 * struct
 *   module V_in = struct
 *     include V
 * 
 *     let equal = [%compare.equal: t]
 *   end
 * 
 *   include Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (V_in) (L)
 *   module L = L
 *   module V = V_in
 * 
 *   module E = struct
 *     include E
 *   end
 * end *)

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
  module State_and_type = struct
    module T = struct
      type t = Abs.t * Type.t [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  let costs = Option_vector.create 128

  let states = Option_vector.create 128

  let types = Option_vector.create 128

  let state_cost_idx =
    lazy
      (Array.init !Global.max_cost ~f:(fun _ ->
           Hashtbl.create (module State_and_type)))

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

  let get s c t =
    let idx = c - 1 in
    [%test_pred: int] ~message:"index out of bounds"
      (fun idx -> idx >= 0 && idx < Array.length (Lazy.force state_cost_idx))
      idx;
    let state_idx = (Lazy.force state_cost_idx).(c - 1) in
    Hashtbl.find state_idx (s, t)

  let set s c t id =
    Hashtbl.set ~key:(s, t) ~data:id (Lazy.force state_cost_idx).(c - 1)

  let of_cost c = Hashtbl.data (Lazy.force state_cost_idx).(c - 1)

  let create s c t =
    match get s c t with
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
        set s c t id;
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

  let output_type id = op id |> Op.ret_type
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

module Unique_queue = struct
  let create m = Hash_queue.create (Base.Hashable.of_key m)

  let enqueue q v = Hash_queue.enqueue_back q v v |> ignore

  let enqueue_all q vs = List.iter vs ~f:(enqueue q)

  let dequeue = Hash_queue.dequeue_front

  let dequeue_back = Hash_queue.dequeue_back
end

let fix_up state =
  let worklist = Unique_queue.create (module Node) in
  let add_work () v = Unique_queue.enqueue worklist v in
  V.iter state ~f:(add_work ());
  let fix_node v =
    Node.match_
      ~args:(fix_up_args () add_work state)
      ~state:(fix_up_states () add_work state)
      v
  in
  let rec loop () =
    match Unique_queue.dequeue_back worklist with
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

let roots g = V.filter g ~f:(fun v -> G.in_degree g v = 0)

let pp fmt g =
  let pp_state fmt v = Fmt.pf fmt "x%d" (State.id v) in
  let pp_args =
    Fmt.list ~sep:(Fmt.any " | ") @@ fun fmt (op, args) ->
    Fmt.pf fmt "%a(%a)" Op.pp op (Fmt.list ~sep:(Fmt.any ", ") pp_state) args
  in

  let work = Unique_queue.create (module State) in
  roots g
  |> List.map ~f:Node.to_state_exn
  |> List.iter ~f:(Unique_queue.enqueue work);

  let rec loop () =
    Unique_queue.dequeue work
    |> Option.iter ~f:(fun v ->
           let args_vs = G.succ g (Node.of_state v) in
           let args =
             List.map args_vs ~f:(fun v ->
                 let args_v = Node.to_args_exn v in
                 (Args.op args_v, inputs g args_v))
           in
           Fmt.pf fmt "%a = %a\n" pp_state v pp_args args;
           List.iter args_vs ~f:(fun v ->
               Unique_queue.enqueue_all work @@ G.succ g v);
           loop ())
  in
  loop ()

module Unshare (G_condensed : sig
  module V : sig
    type t [@@deriving sexp_of]

    val hash : t -> int

    val compare : t -> t -> int

    val kind : t -> [ `Args | `State ]
  end

  module E : sig
    type t = V.t * int * V.t
  end

  type t

  val succ_e : t -> V.t -> E.t list

  val in_degree : t -> V.t -> int

  val iter_vertex : (V.t -> unit) -> t -> unit
end) =
struct
  module V_ref = struct
    module T = struct
      type t = { id : int; node : (G_condensed.V.t[@ignore]) }
      [@@deriving compare, equal, hash, sexp_of]
    end

    include T
    include Comparator.Make (T)

    let create =
      let id_ctr = ref 0 in
      fun node ->
        incr id_ctr;
        { id = !id_ctr; node }

    let vertex x = x.node

    let pp pp_node fmt x = Fmt.pf fmt "%a@%d" pp_node x.node x.id
  end

  (** Represents graphs that can contain multiple instances of the same
       vertex. *)
  module G_replicated = struct
    module Edge = struct
      type t = int [@@deriving compare]

      let default = -1
    end

    include Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (V_ref) (Edge)
  end

  module Port = struct
    module T = struct
      type t = { node : V_ref.t; port : int } [@@deriving compare, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end

  let unshare g =
    let g_replicated = G_replicated.create () in

    let work =
      Hash_queue.create
        {
          hash = G_condensed.V.hash;
          compare = G_condensed.V.compare;
          sexp_of_t = [%sexp_of: G_condensed.V.t];
        }
    in

    let update q key ~f =
      let data' = Hash_queue.lookup q key |> f in
      match Hash_queue.replace q key data' with
      | `Ok -> ()
      | `No_such_key -> Hash_queue.enqueue_back_exn q key data'
    in

    let add_ancestor ancestors port =
      match G_condensed.V.kind port.Port.node.node with
      | `Args -> Set.add ancestors port
      | `State -> ancestors
    in

    let exists_common_ancestor asets =
      Set.union_list (module Port) asets
      |> Set.fold_until
           ~init:(Map.empty (module V_ref))
           ~f:(fun ancestor_ports Port.{ node; port } ->
             match Map.find ancestor_ports node with
             | None -> Continue (Map.set ancestor_ports ~key:node ~data:port)
             | Some port' ->
                 if port = port' then Continue ancestor_ports else Stop true)
           ~finish:(fun _ -> false)
    in

    let rec loop () =
      match Hash_queue.dequeue_front_with_key work with
      | Some (vertex, in_edges) ->
          let any_overlap =
            let _, ancestors = List.unzip in_edges in
            exists_common_ancestor ancestors
          in

          if List.is_empty in_edges then (
            let this_ref = V_ref.create vertex in

            G_replicated.add_vertex g_replicated this_ref;

            G_condensed.succ_e g vertex
            |> List.iter ~f:(fun (_, idx', child) ->
                   let port_ref = Port.{ node = this_ref; port = idx' } in
                   let child_in_edge =
                     (port_ref, add_ancestor (Set.empty (module Port)) port_ref)
                   in
                   update work child ~f:(fun edges ->
                       child_in_edge :: Option.value edges ~default:[])) )
          else if not any_overlap then
            let this_ref = V_ref.create vertex in
            List.iter in_edges
              ~f:(fun (Port.{ node = parent; port = idx }, ancestors) ->
                G_replicated.add_edge_e g_replicated (parent, idx, this_ref);

                G_condensed.succ_e g vertex
                |> List.iter ~f:(fun (_, idx', child) ->
                       let this_port = Port.{ node = this_ref; port = idx' } in
                       let child_in_edge =
                         (this_port, add_ancestor ancestors this_port)
                       in
                       update work child ~f:(fun edges ->
                           child_in_edge :: Option.value edges ~default:[])))
          else
            List.iter in_edges
              ~f:(fun (Port.{ node = parent; port = idx }, ancestors) ->
                let this_ref = V_ref.create vertex in

                G_replicated.add_edge_e g_replicated (parent, idx, this_ref);

                G_condensed.succ_e g vertex
                |> List.iter ~f:(fun (_, idx', child) ->
                       let this_port = Port.{ node = this_ref; port = idx' } in
                       let child_in_edge =
                         (this_port, add_ancestor ancestors this_port)
                       in
                       update work child ~f:(fun edges ->
                           child_in_edge :: Option.value edges ~default:[])));
          loop ()
      | None -> ()
    in

    G_condensed.iter_vertex
      (fun v ->
        if G_condensed.in_degree g v = 0 then
          Hash_queue.enqueue_back_exn work v [])
      g;
    loop ();

    g_replicated
end

let%test_module "unshare" =
  ( module struct
    module G = struct
      module Vertex = Int

      module Edge = struct
        type t = int [@@deriving compare]

        let default = -1
      end

      include Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
                (Vertex)
                (Edge)

      module V = struct
        let sexp_of_t = [%sexp_of: int]

        let kind x = if x mod 2 = 0 then `Args else `State

        include V
      end
    end

    module U = Unshare (G)

    let pp fmt g =
      let pp_vertex = U.V_ref.pp Int.pp in
      U.G_replicated.iter_edges
        (fun v v' -> Fmt.pf fmt "%a -> %a@." pp_vertex v pp_vertex v')
        g

    let%expect_test "" =
      let g = G.create () in
      G.add_edge_e g (0, 0, 1);
      G.add_edge_e g (0, 1, 3);
      G.add_edge g 1 4;
      G.add_edge g 3 4;
      let g' = U.unshare g in
      Fmt.pr "%a" pp g';
      [%expect {|
0@1 -> 3@2
0@1 -> 1@3
3@2 -> 4@5
1@3 -> 4@4 |}]

    let%expect_test "" =
      let g = G.create () in
      G.add_edge_e g (0, 0, 1);
      G.add_edge_e g (0, 1, 3);
      G.add_edge g 1 2;
      G.add_edge g 1 4;
      G.add_edge g 3 4;
      G.add_edge g 3 6;
      let g' = U.unshare g in
      Fmt.pr "%a" pp g';
      [%expect
        {|
1@8 -> 4@10
1@8 -> 2@12
3@7 -> 6@9
3@7 -> 4@11
0@6 -> 3@7
0@6 -> 1@8 |}]

    let%expect_test "" =
      let g = G.create () in
      G.add_edge_e g (0, 0, 1);
      G.add_edge g 1 2;
      G.add_edge g 1 4;
      let g' = U.unshare g in
      Fmt.pr "%a" pp g';
      [%expect
        {|
           0@13 -> 1@14
           1@14 -> 4@15
           1@14 -> 2@16 |}]

    let%expect_test "" =
      let g = G.create () in
      G.add_edge g 0 1;
      G.add_edge g 1 2;
      G.add_edge g 1 4;
      G.add_edge g 2 3;
      G.add_edge g 4 3;
      let g' = U.unshare g in
      Fmt.pr "%a" pp g';
      [%expect
        {|
           2@20 -> 3@21
           4@19 -> 3@21
           1@18 -> 4@19
           1@18 -> 2@20
           0@17 -> 1@18 |}]

    let%expect_test "" =
      let g = G.create () in
      G.add_edge_e g (0, 0, 1);
      G.add_edge_e g (0, 1, 5);
      G.add_edge g 1 2;
      G.add_edge g 1 4;
      G.add_edge g 2 5;
      G.add_edge g 4 5;
      G.add_edge g 5 6;
      G.add_edge g 5 8;
      G.add_edge_e g (8, 0, 9);
      G.add_edge_e g (8, 1, 9);
      let g' = U.unshare g in
      Fmt.pr "%a@." pp g';
      [%expect
        {|
1@24 -> 4@27
1@24 -> 2@28
4@27 -> 5@31
5@23 -> 8@25
5@23 -> 6@26
0@22 -> 5@23
0@22 -> 1@24
2@28 -> 5@31
8@25 -> 9@29
8@25 -> 9@30
8@32 -> 9@34
8@32 -> 9@35
8@32 -> 9@36
8@32 -> 9@37
5@31 -> 8@32
5@31 -> 6@33 |}]
  end )
