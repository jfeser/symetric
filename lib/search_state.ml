open Ast

module Edge = struct
  type t = int [@@deriving compare, hash, sexp]

  let default = -1
end

module G = struct
  module G0 = Graph_ext.Make (Int) (Edge)
  include G0
  module Fold = Graph_ext.Folds (G0)

  include Graph_ext.Changed (G0) ()
end

module Is_fresh = struct
  type 'a t = Fresh of 'a | Stale of 'a

  let is_fresh = function Fresh _ -> true | _ -> false

  let unwrap (Fresh x | Stale x) = x
end

open Is_fresh

module State_and_type = struct
  module T = struct
    type t = Abs.t * Type.t [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)
end

module State0 = struct
  type t = int [@@deriving compare, hash, sexp_of]
end

module Args0 = struct
  type t = int [@@deriving compare, hash, sexp_of]
end

module Hyper_edge0 = struct
  type t = State0.t list * Offset.t Op.t * State0.t
  [@@deriving compare, hash, sexp_of]
end

type t = {
  params : Params.t;
  graph : G.t;
  (* State data *)
  costs : int Option_vector.t;
  states : Abs.t Option_vector.t;
  types : Type.t Option_vector.t;
  state_idx : State0.t Hashtbl.M(State_and_type).t array;
  mutable state_id : int;
  (* Hyper_edge data *)
  hyper_edge_idx : Hyper_edge0.t Hash_set.t;
  (* Args data *)
  ops : Offset.t Op.t Option_vector.t;
  hyper_edges : Hyper_edge0.t Option_vector.t;
  mutable args_id : int;
}

let create params =
  {
    params;
    graph = G.create ();
    costs = Option_vector.create 128;
    states = Option_vector.create 128;
    types = Option_vector.create 128;
    state_idx =
      Array.init params.max_cost ~f:(fun _ ->
          Hashtbl.create (module State_and_type));
    state_id = 1;
    hyper_edge_idx = Hash_set.create (module Hyper_edge0);
    ops = Option_vector.create 128;
    hyper_edges = Option_vector.create 128;
    args_id = 0;
  }

let params ctx = ctx.params

let graph ctx = ctx.graph

module State = struct
  include State0
  include Comparator.Make (State0)

  let state ctx id = Option_vector.get_some_exn ctx.states (id / 2)

  let cost ctx id = Option_vector.get_some_exn ctx.costs (id / 2)

  let type_ ctx id = Option_vector.get_some_exn ctx.types (id / 2)

  let get ctx s c t =
    let idx = c - 1 in
    [%test_pred: int] ~message:"index out of bounds"
      (fun idx -> idx >= 0 && idx < Array.length ctx.state_idx)
      idx;
    Hashtbl.find ctx.state_idx.(c - 1) (s, t)

  let set ctx s c t id = Hashtbl.set ~key:(s, t) ~data:id ctx.state_idx.(c - 1)

  let of_cost ctx c = Hashtbl.data ctx.state_idx.(c - 1)

  let create ctx s c t =
    match get ctx s c t with
    | Some id -> Stale id
    | None ->
        let id = ctx.state_id in
        ctx.state_id <- ctx.state_id + 2;
        let idx = id / 2 in
        Option_vector.reserve ctx.states idx;
        Option_vector.reserve ctx.costs idx;
        Option_vector.reserve ctx.types idx;
        Option_vector.set_some ctx.states idx s;
        Option_vector.set_some ctx.costs idx c;
        Option_vector.set_some ctx.types idx t;
        set ctx s c t id;
        Fresh id

  let id = ident

  let graphviz_pp ctx fmt id =
    Fmt.pf fmt "%a<br/>id=%d cost=%d"
      (Abs.graphviz_pp ctx.params)
      (state ctx id) id (cost ctx id)
end

module Hyper_edge = struct
  include Hyper_edge0

  let mem ctx = Hash_set.mem ctx.hyper_edge_idx

  let add ctx = Hash_set.add ctx.hyper_edge_idx

  let remove ctx = Hash_set.remove ctx.hyper_edge_idx
end

module Args = struct
  include Args0
  include Comparator.Make (Args0)

  let id = ident

  let op ctx id = Option_vector.get_some_exn ctx.ops (id / 2)

  let hyper_edge ctx id = Option_vector.get ctx.hyper_edges (id / 2)

  let set_hyper_edge ctx id = Option_vector.set_some ctx.hyper_edges (id / 2)

  let graphviz_pp ctx fmt x = Fmt.pf fmt "%a<br/>id=%d" Op.pp (op ctx x) (id x)

  let create ctx op =
    let id = ctx.args_id in
    ctx.args_id <- ctx.args_id + 2;
    let idx = id / 2 in
    Option_vector.reserve ctx.ops idx;
    Option_vector.set_some ctx.ops idx op;
    Option_vector.reserve ctx.hyper_edges idx;
    id

  let output_type ctx id = op ctx id |> Op.ret_type
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

  let type_ ctx = match_ ~args:(Args.output_type ctx) ~state:(State.type_ ctx)
end

let args ctx =
  G.Fold.V.filter_map ctx.graph
    ~f:(Node.match_ ~args:Option.some ~state:(fun _ -> None))

let filter ctx ~f =
  G.Fold.V.filter ctx.graph ~f:(Fun.negate f)
  |> List.iter ~f:(fun v ->
         if G.mem_vertex ctx.graph v then G.remove_vertex ctx.graph v)

let nb_vertex ctx = G.nb_vertex ctx.graph

let states_of_cost ctx cost =
  State.of_cost ctx cost |> List.filter ~f:(G.mem_vertex ctx.graph)

let inputs ctx arg_v =
  G.succ_e ctx.graph (Node.of_args arg_v)
  |> List.map ~f:(fun (_, n, v) -> (Node.to_state_exn v, n))
  |> List.sort ~compare:(fun (_, n) (_, n') -> [%compare: int] n n')
  |> List.map ~f:(fun (v, _) -> v)

let remove_args ctx v =
  Option.iter (Args.hyper_edge ctx v) ~f:(Hyper_edge.remove ctx);
  G.remove_vertex ctx.graph (Node.of_args v)

let fix_up_args ctx work add_work v =
  if G.out_degree ctx.graph v <> Op.arity (Args.op ctx v) then (
    let work' = G.fold_pred (fun v' w -> add_work w v') ctx.graph v work in
    remove_args ctx v;
    work' )
  else if G.in_degree ctx.graph v = 0 then (
    remove_args ctx v;
    work )
  else work

let fix_up_states ctx work add_work v =
  if G.out_degree ctx.graph v = 0 then (
    let work' = G.fold_pred (fun v' w -> add_work w v') ctx.graph v work in
    G.remove_vertex ctx.graph v;
    work' )
  else work

module Unique_queue = struct
  let create m = Hash_queue.create (Base.Hashable.of_key m)

  let enqueue q v =
    (Hash_queue.enqueue_back q v v : [ `Key_already_present | `Ok ]) |> ignore

  let enqueue_all q vs = List.iter vs ~f:(enqueue q)

  let dequeue = Hash_queue.dequeue_front

  let dequeue_back = Hash_queue.dequeue_back
end

let fix_up ctx =
  let worklist = Unique_queue.create (module Node) in
  let add_work () v = Unique_queue.enqueue worklist v in
  G.Fold.V.iter ctx.graph ~f:(add_work ());
  let fix_node v =
    Node.match_
      ~args:(fix_up_args ctx () add_work)
      ~state:(fix_up_states ctx () add_work)
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

let insert_hyper_edge ctx state_v_ins op state_v_out =
  let args_v = Args.create ctx op in
  let hyper_edge = (state_v_ins, op, state_v_out) in
  Args.set_hyper_edge ctx args_v hyper_edge;
  Hyper_edge.add ctx hyper_edge;
  List.iteri state_v_ins ~f:(fun i v -> G.add_edge_e ctx.graph (args_v, i, v));
  G.add_edge_e ctx.graph (state_v_out, -1, args_v)

let insert_hyper_edge_if_not_exists ctx state_v_ins op state_v_out =
  let hyper_edge = (state_v_ins, op, state_v_out) in
  if not (Hyper_edge.mem ctx hyper_edge) then
    insert_hyper_edge ctx state_v_ins op state_v_out

let roots g = G.Fold.V.filter g ~f:(fun v -> G.in_degree g v = 0)

let pp fmt ctx =
  let pp_state fmt v = Fmt.pf fmt "x%d" (State.id v) in
  let pp_args =
    Fmt.list ~sep:(Fmt.any " | ") @@ fun fmt (op, args) ->
    Fmt.pf fmt "%a(%a)" Op.pp op (Fmt.list ~sep:(Fmt.any ", ") pp_state) args
  in

  let work = Unique_queue.create (module State) in
  roots ctx.graph
  |> List.map ~f:Node.to_state_exn
  |> List.iter ~f:(Unique_queue.enqueue work);

  let rec loop () =
    Unique_queue.dequeue work
    |> Option.iter ~f:(fun v ->
           let args_vs = G.succ ctx.graph (Node.of_state v) in
           let args =
             List.map args_vs ~f:(fun v ->
                 let args_v = Node.to_args_exn v in
                 (Args.op ctx args_v, inputs ctx args_v))
           in
           Fmt.pf fmt "%a = %a\n" pp_state v pp_args args;
           List.iter args_vs ~f:(fun v ->
               Unique_queue.enqueue_all work @@ G.succ ctx.graph v);
           loop ())
  in
  loop ()

module type ATTR = sig
  val vertex_name : G.V.t -> string

  val vertex_attributes : G.V.t -> Graph.Graphviz.DotAttributes.vertex list
end

let attr ctx =
  ( module struct
    let vertex_name n = Fmt.str "%d" @@ Node.id n

    let vertex_attributes n =
      Node.match_ n
        ~args:(fun n ->
          [ `HtmlLabel (Fmt.str "%a" (Args.graphviz_pp ctx) n); `Shape `Box ])
        ~state:(fun n ->
          [ `HtmlLabel (Fmt.str "%a" (State.graphviz_pp ctx) n) ]
          @
          if
            Abs.contains (State.state ctx n)
            @@ Conc.bool_vector ctx.params.bench.output
          then [ `Style `Bold ]
          else [])
  end : ATTR )

let dump_detailed ?suffix ?cone ?separator ?refinement ?depth ctx =
  let (module Attr) = attr ctx in
  let module D = Dump.Make (G) (Attr) in
  D.dump_detailed ?suffix ?cone ?separator ?refinement ?depth ctx.params
    ctx.graph

let dump_detailed_graph ?suffix ?cone ?separator ?refinement ?depth ctx graph =
  let (module Attr) = attr ctx in
  let module D = Dump.Make (G) (Attr) in
  D.dump_detailed ?suffix ?cone ?separator ?refinement ?depth ctx.params graph

let sample_program ctx =
  let value_exn x =
    Option.value_exn ~here:[%here] ~message:"malformed state space" x
  in
  let rec sample arg =
    let inputs =
      G.succ ctx.graph arg
      |> List.map ~f:(fun state_v ->
             let p, _ =
               G.succ ctx.graph state_v |> List.random_element |> value_exn
               |> sample
             in
             p)
    in
    let op = Args.op ctx arg in
    (`Apply (op, inputs), arg)
  in

  args ctx |> List.random_element |> value_exn |> sample

let validate ?(k = 100) ctx =
  for _ = 1 to k do
    let p, arg_v = sample_program ctx in
    let conc = Program.ceval ctx.params p in
    let abs =
      G.pred ctx.graph arg_v
      |> List.map ~f:(fun state_v ->
             Node.to_state_exn state_v |> State.state ctx)
    in
    let contained = List.exists abs ~f:(fun abs -> Abs.contains abs conc) in
    if not contained then (
      dump_detailed ~suffix:"error" ctx;
      raise_s
        [%message
          "not an overapproximation"
            (p : Program.t)
            (arg_v : Args.t)
            (abs : Abs.t list)
            (conc : Conc.t)] )
  done

let validate ?k:_ _ = ()
