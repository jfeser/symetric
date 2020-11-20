open Ast

module Is_fresh = struct
  type 'a t = Fresh of 'a | Stale of 'a

  let is_fresh = function Fresh _ -> true | _ -> false

  let unwrap (Fresh x | Stale x) = x
end

open Is_fresh

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
  type t = int [@@deriving compare, hash, sexp]

  let default = -1
end

module G = struct
  module G0 = Graph_ext.Make (Node) (Edge)
  include G0
  module Fold = Graph_ext.Folds (G0)

  include Graph_ext.Changed (G0) ()

  let iter_succ_e f g v =
    try iter_succ_e f g v
    with Invalid_argument msg ->
      raise_s [%message "iter_succ_e failed" (msg : string) (v : Node.t)]
end

type t = G.t

let create () = G.create ()

let filter g ~f =
  G.Fold.V.filter g ~f:(Fun.negate f)
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
  G.Fold.V.iter state ~f:(add_work ());
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

let roots g = G.Fold.V.filter g ~f:(fun v -> G.in_degree g v = 0)

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

module Attr = struct
  let vertex_name n = Fmt.str "%d" @@ Node.id n

  let vertex_attributes n =
    Node.match_ n
      ~args:(fun n ->
        [ `HtmlLabel (Fmt.str "%a" Args.graphviz_pp n); `Shape `Box ])
      ~state:(fun n ->
        [ `HtmlLabel (Fmt.str "%a" State.graphviz_pp n) ]
        @
        if
          Abs.contains (State.state n)
          @@ Conc.bool_vector
          @@ (Set_once.get_exn Global.bench [%here]).Bench.output
        then [ `Style `Bold ]
        else [])
end

include Dump.Make (G) (Attr)
