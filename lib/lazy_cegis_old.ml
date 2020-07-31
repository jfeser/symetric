open! Core

module Seq = struct
  include Sequence

  let of_array a = init (Array.length a) ~f:(fun i -> a.(i))
end

let value_exn x = Option.value_exn x

let enable_dump = ref false

let refine_strategy : [ `First | `Random | `Pareto ] ref = ref `First

let max_size = ref 10

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
  type t [@@deriving compare, hash, sexp]

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

  val contains : t -> Conc.t -> bool

  val width : t -> int

  val of_list_exn : (int * bool) list -> t
end

module Map_abs : ABS with type t = bool Map.M(Int).t = struct
  module T = struct
    type t = bool Map.M(Int).t [@@deriving compare, hash, sexp]
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
    Map.merge ~f:(fun ~key:_ ->
      function
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
      | `Both (x, x') -> Some (x && not x')
      | `Left false | `Right true -> Some false
      | `Left true | `Right false -> None)

  let mem v i = Map.mem v i

  let add_exn m k v = Map.add_exn m ~key:k ~data:v

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

  let graphviz_pp fmt { op; _ } = Op.pp fmt op

  let create op = { id = mk_id (); op }

  let id { id; _ } = id

  let op { op; _ } = op
end

module State_node0 = struct
  let time = ref 0

  module T = struct
    type t = {
      id : int;
      mutable cost : int; [@ignore]
      mutable state : Abs.t; [@ignore]
      mutable covered : bool; [@ignore]
      mutable last_mod_time : int; [@ignore]
    }
    [@@deriving compare, equal, hash, sexp, show]
  end

  include T
  include Comparator.Make (T)

  let id { id; _ } = id

  let state { state; _ } = state

  let set_state n s = n.state <- s

  let cover n =
    if not n.covered then (
      incr time;
      n.covered <- true;
      n.last_mod_time <- !time )

  let uncover n =
    if n.covered then (
      incr time;
      n.covered <- false;
      n.last_mod_time <- !time )

  let pp fmt { state; _ } = Abs.pp fmt state

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
  module G = struct
    include Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node) (Edge)

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
  include Graph.Graphviz.Dot (G)

  let iter_state_vertex g ~f =
    iter_vertex (function State x -> f x | _ -> ()) g

  (** The set of nodes that depend on a state 'v'. *)
  let depends g v =
    pred g (State v)
    |> List.concat_map ~f:(pred g)
    |> List.dedup_and_sort ~compare:[%compare: Node.t]
    |> List.map ~f:(function Node.State x -> x | Args _ -> assert false)

  let ensure_vertex g v = if not (mem_vertex g v) then add_vertex g v

  let ensure_edge_e g e = if not (mem_edge_e g e) then add_edge_e g e
end

module Search_state = struct
  module Args_table_key = struct
    module T = struct
      type t = Op.t * State_node0.t list [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  type t = {
    graph : G.t;
    args_table : Args_node0.t Hashtbl.M(Args_table_key).t;
    state_table : State_node0.t Hashtbl.M(Abs).t;
  }

  let create () =
    {
      graph = G.create ();
      args_table = Hashtbl.create (module Args_table_key);
      state_table = Hashtbl.create (module Abs);
    }

  let copy x =
    {
      graph = G.copy x.graph;
      args_table = Hashtbl.copy x.args_table;
      state_table = Hashtbl.copy x.state_table;
    }

  let wrap f g = f g.graph

  module V = struct
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

    include G.E
  end

  let succ = wrap G.succ

  let pred = wrap G.pred

  let succ_e = wrap G.succ_e

  let pred_e = wrap G.pred_e

  let ensure_edge_e = wrap G.ensure_edge_e

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

  let remove_vertex x v =
    G.remove_vertex x.graph v;
    Hashtbl.filter_inplace x.args_table ~f:(fun v' -> Node.O.(v <> Args v'));
    Hashtbl.filter_inplace x.state_table ~f:(fun v' -> Node.O.(v <> State v'))

  let filter g ~f =
    G.iter_vertex (fun v -> if not (f v) then remove_vertex g v) g.graph;
    Hashtbl.filter_inplace g.args_table ~f:(fun v -> f (Args v));
    Hashtbl.filter_inplace g.state_table ~f:(fun v -> f (State v))
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

(* let refine_ordered strong_enough abs conc =
 *   let open State_node0 in
 *   let abs = Array.of_list abs and conc = Array.of_list conc in
 *   let n = Array.length conc and k = Array.length conc.(0) in
 *   let rec loop i j =
 *     if i >= n || j >= k then failwith "Could not refine";
 * 
 *     if not (Abs.mem abs.(i) j) then
 *       abs.(i) <- Abs.add_exn abs.(i) j conc.(i).(j);
 * 
 *     if not (strong_enough @@ Array.to_list abs) then
 *       if j = k - 1 then loop (i + 1) 0 else loop i (j + 1)
 *   in
 *   loop 0 0;
 *   Array.to_list abs
 * 
 * let refine_random ?(state = Random.State.default) strong_enough abs conc =
 *   if strong_enough abs then abs
 *   else
 *     let abs = Array.of_list abs and conc = Array.of_list conc in
 *     let n = Array.length conc and k = Array.length conc.(0) in
 *     let choices =
 *       List.init n ~f:(fun i ->
 *           List.init k ~f:(fun j ->
 *               if Abs.mem abs.(i) j then None else Some (i, j))
 *           |> List.filter_map ~f:Fun.id)
 *       |> List.concat
 *       |> List.permute ~random_state:state
 *       |> Queue.of_list
 *     in
 *     let rec refine () =
 *       let i, j =
 *         match Queue.dequeue choices with
 *         | Some x -> x
 *         | None -> failwith Fmt.(str "cannot refine %a" (Dump.array Abs.pp) abs)
 *       in
 *       abs.(i) <- Abs.add_exn abs.(i) j conc.(i).(j);
 *       let abs_list = Array.to_list abs in
 *       if strong_enough abs_list then abs_list else refine ()
 *     in
 *     refine ()
 * 
 * let refine_pareto strong_enough abs conc =
 *   if strong_enough abs then abs
 *   else
 *     let abs_a = Array.of_list abs and conc = Array.of_list conc in
 *     let n = Array.length conc and k = Array.length conc.(0) in
 *     let choices =
 *       List.init n ~f:(fun i ->
 *           List.init k ~f:(fun j ->
 *               if Abs.mem abs_a.(i) j then None else Some (i, j))
 *           |> List.filter_map ~f:Fun.id)
 *       |> List.concat
 *     in
 *     let rec refine n_choices =
 *       if n_choices > List.length choices then failwith "cannot refine";
 *       match
 *         Combinat.Combination.Of_list.(
 *           create choices n_choices
 *           |> find_map ~f:(fun cs ->
 *                  let abs = Array.of_list abs in
 *                  List.iter cs ~f:(fun (i, j) ->
 *                      abs.(i) <- Abs.add_exn abs.(i) j conc.(i).(j));
 *                  let abs = Array.to_list abs in
 *                  if strong_enough abs then Some abs else None))
 *       with
 *       | Some abs -> abs
 *       | None -> refine (n_choices + 1)
 *     in
 *     refine 1
 * 
 * let refine_hybrid strong_enough abs conc =
 *   if strong_enough abs then abs
 *   else
 *     let abs_a = Array.of_list abs and conc_a = Array.of_list conc in
 *     let n = Array.length conc_a and k = Array.length conc_a.(0) in
 *     let choices =
 *       List.init n ~f:(fun i ->
 *           List.init k ~f:(fun j ->
 *               if Abs.mem abs_a.(i) j then None else Some (i, j))
 *           |> List.filter_map ~f:Fun.id)
 *       |> List.concat
 *     in
 *     let rec refine n_choices =
 *       if n_choices > 4 then refine_ordered strong_enough abs conc
 *       else
 *         match
 *           Combinat.Combination.Of_list.(
 *             create choices n_choices
 *             |> find_map ~f:(fun cs ->
 *                    let abs = Array.of_list abs in
 *                    List.iter cs ~f:(fun (i, j) ->
 *                        abs.(i) <- Abs.add_exn abs.(i) j conc_a.(i).(j));
 *                    let abs = Array.to_list abs in
 *                    if strong_enough abs then Some abs else None))
 *         with
 *         | Some abs -> abs
 *         | None -> refine (n_choices + 1)
 *     in
 *     refine 1
 * 
 * let prune g (nodes : State_node0.t list) =
 *   let open State_node0 in
 *   if not (List.is_empty nodes) then (
 *     let min_mod_time =
 *       Option.value_exn
 *         ( List.map nodes ~f:(fun n -> n.last_mod_time)
 *         |> List.min_elt ~compare:[%compare: int] )
 *     in
 *     G.iter_state_vertex
 *       ~f:(fun v -> if v.last_mod_time > min_mod_time then uncover v)
 *       g;
 *     Fmt.epr "Prune: min mod time %d.\n" min_mod_time;
 * 
 *     let rec process = function
 *       | v :: vs ->
 *           let work =
 *             (\* Select the arg nodes that depend on this state. *\)
 *             G.pred g (Node.State v)
 *             |> List.concat_map ~f:(function
 *                  | Node.Args a ->
 *                      let state = eval g a in
 * 
 *                      (\* Push the state update forward through the args nodes. *\)
 *                      List.concat_map (G.pred g (Args a)) ~f:(function
 *                        | State v' ->
 *                            let old = v'.state in
 *                            let new_ = Abs.meet old state in
 *                            if [%compare.equal: Abs.t] old new_ then []
 *                            else (
 *                              set_state v' new_;
 *                              v' :: refine_children g v' )
 *                        | Args _ -> failwith "expected a state node")
 *                  | State _ -> failwith "expected an args node")
 *           in
 *           process (work @ vs)
 *       | [] -> ()
 *     in
 *     process nodes )
 * 
 * let update_covers graph =
 *   let uncovered =
 *     G.filter_map_vertex graph ~f:(function
 *       | State v when not v.covered -> Some v
 *       | _ -> None)
 *   in
 *   List.iter uncovered ~f:(fun v ->
 *       if
 *         List.exists uncovered ~f:(fun v' ->
 *             (not v'.covered) && v'.cost <= v.cost
 *             && (not ([%compare.equal: State_node0.t] v v'))
 *             && Abs.is_subset_a v'.state ~of_:v.state)
 *       then State_node0.cover v) *)

module Args_node = struct
  include Args_node0

  let create ~op graph args =
    match Hashtbl.find graph.Search_state.args_table (op, args) with
    | Some v -> None
    | None ->
        let args_n = { op; id = mk_id () } in
        let args_v = Node.Args args_n in
        List.iteri args ~f:(fun i v ->
            S.ensure_edge_e graph (args_v, i, Node.State v));
        Hashtbl.set graph.args_table (op, args) args_n;
        Some args_n
end

module State_node = struct
  include State_node0

  let rec choose_program graph node =
    match S.children graph node with
    | (op, args) :: _ -> `Apply (op, List.map args ~f:(choose_program graph))
    | _ -> failwith "expected arguments"

  let create ?(covered = false) ~state ~cost (g : Search_state.t) =
    match Hashtbl.find g.state_table state with
    | Some v ->
        v.cost <- Int.min v.cost cost;
        v
    | None ->
        let v = { id = mk_id (); cost; state; covered; last_mod_time = -1 } in
        Hashtbl.add_exn g.state_table state v;
        v

  let create_op ?(covered = false) ~state ~cost ~op g children =
    match Args_node.create ~op g children with
    | Some args_v ->
        let state_v = create ~covered ~state ~cost g in
        S.ensure_edge_e g (Node.State state_v, -1, Node.Args args_v);
        true
    | None -> false
end

let rec fill graph cost =
  if cost <= 1 then false
  else if fill graph (cost - 1) then true
  else
    let arg_cost = cost - 1 in
    let module Part = Combinat.Partition in
    let module Perm = Combinat.Permutation.Of_list in
    let of_cost c =
      S.V.filter_map
        ~f:(function
          | State v when (not v.covered) && v.cost = c -> Some v | _ -> None)
        graph
    in

    let added = ref false in
    Part.create ~n:arg_cost ~parts:2
    |> Part.iter ~f:(fun arg_costs ->
           let arg_costs =
             List.init (Bigarray.Array1.dim arg_costs) ~f:(fun i ->
                 arg_costs.{i})
           in
           Perm.(create arg_costs |> to_list)
           |> List.dedup_and_sort ~compare:[%compare: int list]
           |> List.iter ~f:(fun arg_costs ->
                  match arg_costs with
                  | [ c; c' ] ->
                      of_cost c
                      |> List.iter ~f:(fun (a : State_node.t) ->
                             of_cost c'
                             |> List.iter ~f:(fun (a' : State_node.t) ->
                                    List.iter [ Op.Union; Inter; Sub ]
                                      ~f:(fun op ->
                                        let state =
                                          match op with
                                          | Op.Union ->
                                              Abs.union a.state a'.state
                                          | Inter -> Abs.inter a.state a'.state
                                          | Sub -> Abs.sub a.state a'.state
                                          | _ -> assert false
                                        in
                                        let did_add =
                                          State_node.create_op ~state ~cost ~op
                                            graph [ a; a' ]
                                        in
                                        added := !added || did_add)))
                  | _ -> failwith "Unexpected costs"));
    !added

(* let refine graph (node : State_node.t) bad =
 *   let conc = node.cstate and old = node.state in
 * 
 *   (\* The bad behavior should not be the same as the concrete behavior, the
 *      initial state should abstract the bad behavior and the concrete behavior.
 *   *\)
 *   assert (
 *     (not ([%compare.equal: Conc.t] bad conc))
 *     && Abs.contains old bad && Abs.contains old conc );
 * 
 *   let len = Array.length conc in
 *   let rec loop i =
 *     if i >= len then failwith "Could not refine"
 *     else if Bool.(conc.(i) <> bad.(i)) then Abs.add_exn node.state i conc.(i)
 *     else loop (i + 1)
 *   in
 *   let new_ = loop 0 in
 * 
 *   (\* Check that the new state refines the old one, does not contain the bad
 *      state, and still abstracts the concrete behavior. *\)
 *   assert (Abs.is_subset_a old ~of_:new_);
 *   assert (Abs.(not (contains new_ bad)));
 *   assert (Abs.contains new_ conc);
 * 
 *   State_node.set_state node new_;
 *   [ node ]
 * 
 * let rec strengthen graph (node : State_node.t) bad_out =
 *   assert (Abs.contains node.state bad_out);
 *   let to_prune = refine graph node bad_out in
 *   let to_prune' = refine_children graph node in
 *   assert (not (Abs.contains node.state bad_out));
 *   to_prune @ to_prune' *)

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

let dump =
  let step = ref 0 in
  fun ?suffix ?output ?(cone = fun _ -> false) ?(separator = fun _ -> false)
      graph ->
    let module Viz = Graph.Graphviz.Dot (struct
      include G

      let graph_attributes _ = []

      let default_vertex_attributes _ = []

      let vertex_name n = Fmt.str "%d" @@ Node.id n

      let vertex_attributes n =
        let attrs = if cone n then [ `Style `Filled ] else [] in
        let attrs = if separator n then `Style `Dotted :: attrs else attrs in
        let attrs' =
          match n with
          | Node.State n ->
              [ `HtmlLabel (Fmt.str "%a" State_node.graphviz_pp n) ]
              @
              if n.covered then [ `Style `Dotted ]
              else if
                Option.map output ~f:(Abs.contains n.state)
                |> Option.value ~default:false
              then [ `Style `Bold ]
              else []
          | Args n ->
              [ `HtmlLabel (Fmt.str "%a" Args_node.graphviz_pp n); `Shape `Box ]
        in
        attrs @ attrs'

      let get_subgraph _ = None

      let default_edge_attributes _ = []

      let edge_attributes (_, i, _) =
        if i >= 0 then [ `Label (sprintf "%d" i) ] else []
    end) in
    if !enable_dump then (
      let fn =
        let suffix =
          Option.map suffix ~f:(sprintf "-%s") |> Option.value ~default:""
        in
        sprintf "%d-graph%s.dot" !step suffix
      in
      Out_channel.with_file fn ~f:(fun ch ->
          Viz.output_graph ch graph.Search_state.graph);
      incr step )

let separators graph target =
  Seq.unfold ~init:(S.succ graph target) ~f:(fun sep ->
      if List.is_empty sep then None
      else
        let sep' =
          List.concat_map sep ~f:(S.succ graph)
          |> List.concat_map ~f:(S.succ graph)
        in
        Some (sep, sep'))

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
  fun v -> target_reaches v && (is_separator v || not (separator_reaches v))

let get_refinement vectors graph target_node expected_output separator =
  let module Smt = Smt.Make () in
  (* Select the subset of the graph that can reach the target. *)
  let graph =
    let in_cone = in_cone graph target_node separator in
    let g = S.copy graph in
    S.filter g ~f:in_cone;
    g
  in
  dump ~suffix:"cone" graph;

  let edge_vars =
    S.E.to_list graph
    |> List.map ~f:(fun e -> (e, Smt.fresh_decl ~prefix:"e" ()))
    |> Map.of_alist_exn (module S.E)
  and state_vars =
    S.V.filter_map graph ~f:Node.to_state
    |> List.map ~f:(fun (v : State_node.t) ->
           let vars =
             List.map vectors ~f:(fun vec ->
                 match Map.find v.state vec with
                 | Some x -> Smt.Bool.bool x
                 | None ->
                     Smt.fresh_decl ~prefix:(sprintf "s%d_b%d_" v.id vec) ())
           in
           (v, vars))
    |> Map.of_alist_exn (module State_node)
  and arg_out_vars =
    S.V.filter_map graph ~f:Node.to_args
    |> List.map ~f:(fun v ->
           let vars =
             List.init (List.length vectors) ~f:(fun b ->
                 Smt.fresh_decl ~prefix:(sprintf "out_b%d_" b) ())
           in
           (v, vars))
    |> Map.of_alist_exn (module Args_node)
  in

  let sep_group = Smt.Interpolant.Group.create () in

  (* The output state must have the expected value. *)
  List.map2_exn (Map.find_exn state_vars target_node)
    (Array.to_list expected_output) ~f:(fun x v -> Smt.Bool.(x = bool v))
  |> Smt.Bool.and_
  |> Smt.make_defn "correct-output"
  |> Smt.Interpolant.assert_group;

  (* Every selected state node must select exactly one dependent args node. *)
  S.V.filter_map graph ~f:Node.to_state
  |> List.iter ~f:(fun v ->
         let selected =
           if [%equal: State_node.t] target_node v then [ Smt.Bool.(bool true) ]
           else S.pred_e graph (State v) |> List.map ~f:(Map.find_exn edge_vars)
         in
         let deps =
           S.succ_e graph (State v) |> List.map ~f:(Map.find_exn edge_vars)
         in
         if not (List.is_empty deps) then
           Smt.(
             make_defn
               (sprintf "state-%d-deps" v.id)
               Bool.(or_ selected => exactly_one deps)
             |> Interpolant.assert_group));

  (* An arg node with a selected outgoing edge must select all its incoming
     edges. *)
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
  |> Smt.Interpolant.assert_group;

  S.V.filter_map graph ~f:Node.to_state
  |> List.map ~f:(fun v ->
         let state_in = Map.find_exn state_vars v in
         S.succ_e graph (State v)
         |> List.filter_map ~f:(fun ((_, _, v) as e) ->
                Node.to_args v
                |> Option.map ~f:(fun v ->
                       (Map.find_exn arg_out_vars v, Map.find_exn edge_vars e)))
         |> List.map ~f:(fun (args_out, is_selected) ->
                Smt.Bool.(
                  is_selected => and_ (List.map2_exn args_out state_in ~f:( = ))))
         |> Smt.Bool.and_)
  |> Smt.Bool.and_
  |> Smt.make_defn "semantics-connected"
  |> Smt.Interpolant.assert_group;

  S.V.filter_map graph ~f:Node.to_args
  |> List.iter ~f:(fun v ->
         let incoming_edges = S.succ_e graph (Args v) in

         let incoming_states =
           List.map incoming_edges ~f:(fun (_, n, v) ->
               (Node.to_state_exn v, n))
           |> List.sort ~compare:(fun (_, n) (_, n') -> [%compare: int] n n')
           |> List.map ~f:(fun (v, _) -> v)
           |> List.map ~f:(Map.find_exn state_vars)
         in

         let out = Map.find_exn arg_out_vars v in
         let semantic =
           match (v.op, incoming_states) with
           | Union, [ s; s' ] ->
               List.map3_exn out s s' ~f:(fun x y z -> Smt.Bool.(x = (y || z)))
           | Inter, [ s; s' ] ->
               List.map3_exn out s s' ~f:(fun x y z -> Smt.Bool.(x = (y && z)))
           | Sub, [ s; s' ] ->
               List.map3_exn out s s' ~f:(fun x y z ->
                   Smt.Bool.(x = (y && not z)))
           | Input conc, [] ->
               List.map2_exn out (Array.to_list conc) ~f:(fun x v ->
                   Smt.Bool.(x = bool v))
           | op, states ->
               Error.create "Unexpected op." (op, states)
                 [%sexp_of: Op.t * Sexp.t list list]
               |> Error.raise
         in

         let defn =
           Smt.make_defn
             (Fmt.str "semantics-%a-%d" Op.pp v.op v.id)
             (Smt.Bool.and_ semantic)
         in
         if List.mem separator (Args v) ~equal:[%equal: Node.t] then
           Smt.Interpolant.assert_group ~group:sep_group defn
         else Smt.Interpolant.assert_group defn);

  let rec parse_interpolant =
    let open Sexp in
    function
    | List (Atom "and" :: args) -> `And (List.map ~f:parse_interpolant args)
    | List [ Atom "not"; Atom x ] -> `Not (`Var x)
    | Atom x -> `Var x
    | inter ->
        Error.create "Unexpected interpolant" inter [%sexp_of: Sexp.t]
        |> Error.raise
  in

  let rec process_interpolant = function
    | `And xs ->
        List.map xs ~f:process_interpolant
        |> List.reduce
             ~f:
               (Map.merge ~f:(fun ~key:_ ->
                  function
                  | `Both (a, b) ->
                      if Bool.(a = b) then Some a else assert false
                  | `Left a | `Right a -> Some a))
        |> Option.value ~default:(Map.empty (module String))
    | `Not (`Var x) -> Map.singleton (module String) x false
    | `Var x -> Map.singleton (module String) x true
  in

  let open Option.Let_syntax in
  let%map interpolant = Smt.Interpolant.get_interpolant [ sep_group ] in
  Fmt.epr "Got interpolant!\n";
  let position = process_interpolant @@ parse_interpolant interpolant in
  let refinement =
    separator
    |> List.filter_map ~f:Node.to_args
    |> List.map ~f:(fun v ->
           let output_state =
             (S.pred graph (Args v) |> List.hd_exn |> Node.to_state_exn).state
           in
           let out_vars = Map.find_exn arg_out_vars v in
           List.filter_mapi out_vars ~f:(fun i var ->
               let%map pos =
                 match Map.find output_state i with
                 | Some x -> Some x
                 | None ->
                     let var =
                       match var with Atom x -> x | _ -> assert false
                     in
                     Map.find position var
               in
               (i, pos))
           |> Map.of_alist_exn (module Int))
  in
  Fmt.epr "Returning refinement!\n";
  refinement

let value_exn x = Option.value_exn x

let prune graph separator =
  let separator = Set.of_list (module Node) separator in
  let module R = Inv_reachable (G) in
  let reaches_separator =
    R.analyze (Set.mem separator) graph.Search_state.graph
  in
  (* remove everything that can reach the separator *)
  S.filter graph ~f:(fun v -> Set.mem separator v || not (reaches_separator v))

let arg_cost graph arg =
  S.succ graph (Args arg)
  |> List.filter_map ~f:Node.to_state
  |> List.sum (module Int) ~f:(fun v -> v.State_node.cost)
  |> fun cost -> cost + 1

let synth ?(no_abstraction = false) inputs output =
  let graph = S.create () and n_refuted = ref 0 in
  let vectors = List.init (Array.length @@ List.hd_exn inputs) ~f:Fun.id in

  let refute () =
    match
      S.V.find_map graph ~f:(function
        | State v when (not v.covered) && Abs.contains v.state output -> Some v
        | _ -> None)
    with
    | Some target ->
        let refinement, separator =
          separators graph (State target)
          |> Seq.find_map ~f:(fun sep ->
                 let open Option.Let_syntax in
                 dump ~cone:(in_cone graph target sep)
                   ~separator:(List.mem sep ~equal:[%equal: Node.t])
                   ~output ~suffix:"before-refinement" graph;
                 let%map r = get_refinement vectors graph target output sep in
                 (r, sep))
          |> value_exn
        in

        prune graph separator;

        dump
          ~separator:(List.mem separator ~equal:[%equal: Node.t])
          ~output ~suffix:"after-pruning" graph;

        let sep = List.filter_map separator ~f:Node.to_args in

        List.iter2_exn sep refinement ~f:(fun arg_v state ->
            let cost = arg_cost graph arg_v in
            let state_v = State_node.create ~state ~cost graph in
            S.ensure_edge_e graph (State state_v, -1, Args arg_v));

        dump
          ~cone:(reachable graph (State target))
          ~output ~suffix:"after-refinement" graph;

        incr n_refuted;
        true
    | None -> false
  in

  let rec loop cost =
    if cost > !max_size then raise (Done `Unsat);
    let rec strengthen_and_cover () =
      let did_strengthen = refute () in
      if did_strengthen then strengthen_and_cover ()
    in
    strengthen_and_cover ();

    let changed = fill graph cost in
    Fmt.epr "Changed: %b Cost: %d\n%!" changed cost;
    if changed then dump ~output ~suffix:"after-fill" graph;
    let cost = if changed then cost else cost + 1 in
    loop cost
  in

  (* Add inputs to the state space graph. *)
  List.iter inputs ~f:(fun input ->
      let state = if no_abstraction then Abs.lift input else Abs.top in
      State_node.create_op ~state ~cost:1 ~op:(Op.Input input) graph []
      |> ignore);
  dump ~output graph;

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
          n_covered =
            S.V.filter graph ~f:(function State v -> v.covered | _ -> false)
            |> List.length;
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
    if Program.size p > !max_size then sample_prog () else p
  in
  sample_prog ()

let check_search_space ?(n = 100_000) inputs graph =
  let rec loop i =
    if i > n then Ok ()
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
