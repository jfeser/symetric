open! Core

module Cad_bench = struct
  module Serial = struct
    type vector_3 = float * float * float [@@deriving sexp]

    type vector_4 = float * float * float * float [@@deriving sexp]

    type offsets = (int * float) list [@@deriving sexp]

    type t =
      (* spheres *)
      vector_4 list
      * (* vectors *)
      vector_3 list list
      (* cylinders *)
      * (int * vector_4 * (float * float)) list
      (* cylinder offsets *)
      * offsets (* cuboids *)
      * (int * float * float * float) list
      * offsets
      * offsets
      * offsets (* output *)
      * int list
    [@@deriving sexp]
  end

  type cylinder = {
    rot : Vector3.t;
    r : float;
    x : float list;
    y : float;
    z : float;
  }

  type cuboid = {
    rot : Vector3.t;
    x : float list;
    y : float list;
    z : float list;
  }

  type t = {
    io : (Vector3.t * bool) list;
    spheres : (Vector3.t * float) list;
    cylinders : cylinder list;
    cuboids : cuboid list;
    repeats : Vector3.t list;
    max_repeat : int;
  }

  let t_of_sexp s =
    let ( spheres,
          inputs,
          cylinders,
          cyl_offsets,
          cuboids,
          x_offsets,
          y_offsets,
          z_offsets,
          outputs ) =
      [%of_sexp: Serial.t] s
    in
    let offsets id xs =
      List.filter_map xs ~f:(fun (id', x) -> if id = id' then Some x else None)
    in
    assert (List.length inputs = 1);
    let inputs = List.hd_exn inputs in
    {
      io = List.map2_exn inputs outputs ~f:(fun i o -> (i, o > 0));
      spheres = List.map spheres ~f:(fun (x, y, z, r) -> ((x, y, z), r));
      cylinders =
        List.map cylinders ~f:(fun (id, (x_rot, y_rot, z_rot, r), (y, z)) ->
            { rot = (x_rot, y_rot, z_rot); r; y; z; x = offsets id cyl_offsets });
      cuboids =
        List.map cuboids ~f:(fun (id, x_rot, y_rot, z_rot) ->
            {
              rot = (x_rot, y_rot, z_rot);
              x = offsets id x_offsets;
              y = offsets id y_offsets;
              z = offsets id z_offsets;
            });
      repeats = [];
      max_repeat = 0;
    }
end

module Seq = struct
  include Sequence

  let of_array a = init (Array.length a) ~f:(fun i -> a.(i))
end

let value_exn x = Option.value_exn x

let enable_dump = ref false

let refine_strategy : [ `First | `Random | `Pareto ] ref = ref `First

let max_size = ref 10

module type ABS = sig
  type t [@@deriving compare, sexp]

  val top : t

  val pp : t Fmt.t

  val graphviz_pp : t Fmt.t

  val meet : t -> t -> t

  val is_subset : t -> of_:t -> bool

  val union : t -> t -> t

  val inter : t -> t -> t

  val sub : t -> t -> t

  val mem : t -> Vector3.t -> bool

  val add : t -> Vector3.t -> bool -> t

  val width : t -> int

  val of_list : (Vector3.t * bool) list -> t

  val ( ==> ) : t -> t -> bool

  val domain : t -> Set.M(Vector3).t
end

module Map_abs = struct
  module T = struct
    type t = { pos : Set.M(Vector3).t; neg : Set.M(Vector3).t }
    [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let top =
    { pos = Set.empty (module Vector3); neg = Set.empty (module Vector3) }

  let pp = Fmt.nop

  let graphviz_pp = Fmt.nop

  let meet { pos = p1; neg = n1 } { pos = p2; neg = n2 } =
    { pos = Set.inter p1 p2; neg = Set.inter n1 n2 }

  let is_subset s ~of_:s' =
    Set.is_subset s.pos ~of_:s'.pos && Set.is_subset s.neg ~of_:s'.neg

  let lift s =
    Array.mapi s ~f:(fun i x -> (i, x))
    |> Array.to_list
    |> Map.of_alist_exn (module Int)

  let union { pos = p1; neg = n1 } { pos = p2; neg = n2 } =
    { pos = Set.union p1 p2; neg = Set.inter n1 n2 }

  let inter { pos = p1; neg = n1 } { pos = p2; neg = n2 } =
    { pos = Set.inter p1 p2; neg = Set.union n1 n2 }

  let negate { pos; neg } = { neg = pos; pos = neg }

  let sub x y = inter x (negate y)

  let mem a i = Set.mem a.pos i || Set.mem a.neg i

  let add a k v =
    if v then { a with pos = Set.add a.pos k }
    else { a with neg = Set.add a.neg k }

  let ( ==> ) s s' = is_subset s ~of_:s'

  let width { pos; neg } = Set.length pos + Set.length neg

  let of_list =
    List.fold_left ~init:top ~f:(fun a (v, x) ->
        if x then { a with pos = Set.add a.pos v }
        else { a with neg = Set.add a.neg v })

  let domain { pos; neg } = Set.union pos neg
end

module Abs = Map_abs

module Op = struct
  type cuboid = { id : int; rot : Vector3.t } [@@deriving compare, hash, sexp]

  type t =
    | Union
    | Inter
    | Sub
    | Repeat of Vector3.t
    | Repeat_n of int
    | Sphere of (Vector3.t * float)
    | Cuboid of cuboid
    | Cuboid_plane of cuboid * Vector3.dim * float
  [@@deriving compare, hash, sexp]

  type tag = Shape | Count | Plane [@@deriving compare, equal, hash, sexp]

  let pp fmt op =
    let str =
      match op with
      | Union -> "or"
      | Inter -> "and"
      | Sub -> "sub"
      | Repeat _ -> "rep"
      | Repeat_n _ -> "repn"
      | Sphere _ -> "sphere"
      | Cuboid _ -> "cuboid"
      | Cuboid_plane _ -> "offset"
    in
    Fmt.pf fmt "%s" str

  let num_args = function
    | Union | Inter | Sub | Repeat _ -> 2
    | Cuboid _ -> 3
    | Sphere _ | Cuboid_plane _ | Repeat_n _ -> 0

  let is_shape = function
    | Union | Inter | Sub | Repeat _ | Sphere _ | Cuboid _ -> true
    | Repeat_n _ | Cuboid_plane _ -> false

  let tag = function
    | Union | Inter | Sub | Repeat _ | Sphere _ | Cuboid _ -> Shape
    | Repeat_n _ -> Count
    | Cuboid_plane _ -> Plane

  let valid_args op args =
    match (op, args) with
    | (Union | Inter | Sub), [ x; y ] -> is_shape x && is_shape y
    | Repeat _, [ Repeat_n _; x ] -> is_shape x
    | ( Cuboid { id; _ },
        [
          Cuboid_plane ({ id = id1; _ }, X, _);
          Cuboid_plane ({ id = id2; _ }, Y, _);
          Cuboid_plane ({ id = id3; _ }, Z, _);
        ] ) ->
        id = id1 && id = id2 && id = id3
    | _ -> false
end

let mk_id =
  let ctr = ref 0 in
  fun () ->
    incr ctr;
    !ctr

module Args_node0 = struct
  type t = { id : int; op : Op.t } [@@deriving compare, hash, sexp]

  let graphviz_pp fmt { op; _ } = Op.pp fmt op

  let create op = { id = mk_id (); op }

  let id { id; _ } = id

  let op { op; _ } = op
end

module State_node0 = struct
  let time = ref 0

  type t = { id : int; tag : Op.tag; mutable cost : int; mutable state : Abs.t }
  [@@deriving sexp]

  let copy ?cost ?state n =
    {
      id = mk_id ();
      tag = n.tag;
      cost = Option.value cost ~default:n.cost;
      state = Option.value state ~default:n.state;
    }

  let id { id; _ } = id

  let state { state; _ } = state

  let set_state n s = n.state <- s

  let pp fmt { state; _ } = Abs.pp fmt state

  let graphviz_pp fmt { state; cost; _ } =
    Fmt.pf fmt "%a %d" Abs.graphviz_pp state cost

  let hash n = [%hash: int] n.id

  let hash_fold_t state n = [%hash_fold: int] state n.id

  let equal n n' = [%compare.equal: int] n.id n'.id

  let compare n n' = [%compare: int] n.id n'.id
end

module Node = struct
  type t = Args of Args_node0.t | State of State_node0.t
  [@@deriving compare, hash, sexp]

  let equal = [%compare.equal: t]

  let id = function Args x -> Args_node0.id x | State x -> State_node0.id x

  let to_args = function Args x -> x | _ -> failwith "expected an args node"
end

module E = struct
  type t = int [@@deriving compare]

  let compare = [%compare: t]

  let default = -1
end

module G = struct
  module G = struct
    include Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node) (E)

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

  let filter_vertex g ~f =
    fold_vertex (fun x xs -> if f x then x :: xs else xs) g []

  let filter_map_vertex g ~f =
    fold_vertex
      (fun x xs -> match f x with Some x' -> x' :: xs | None -> xs)
      g []

  let map_vertex g ~f = fold_vertex (fun x xs -> f x :: xs) g []

  let exists_vertex g ~f = fold_vertex (fun x xs -> f x || xs) g false

  let find_vertex g ~f =
    fold_vertex
      (fun x acc -> if Option.is_none acc && f x then Some x else acc)
      g None

  let find_map_vertex g ~f =
    fold_vertex (fun x acc -> if Option.is_none acc then f x else acc) g None

  let children g v =
    succ g (State v)
    |> List.map ~f:(fun v' ->
           let a = Node.to_args v' in
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

  (** The set of nodes that depend on a state 'v'. *)
  let depends g v =
    pred g (State v)
    |> List.concat_map ~f:(pred g)
    |> List.dedup_and_sort ~compare:[%compare: Node.t]
    |> List.map ~f:(function Node.State x -> x | Args _ -> assert false)

  let ensure_vertex g v = if not (mem_vertex g v) then add_vertex g v

  let ensure_edge_e g e = if not (mem_edge_e g e) then add_edge_e g e

  let split_vertex g ~orig ~left ~right =
    iter_pred_e
      (fun (pred, e, orig') ->
        [%test_result: Node.t] ~expect:orig orig';
        add_edge_e g (pred, e, left);
        add_edge_e g (pred, e, right))
      g orig;
    iter_succ_e
      (fun (orig', e, succ) ->
        [%test_result: Node.t] ~expect:orig orig';
        add_edge_e g (left, e, succ);
        add_edge_e g (right, e, succ))
      g orig;
    remove_vertex g orig
end

module Program = struct
  module T = struct
    type t = Apply of Op.t * t list [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let eval_open eval term =
    match (term#op, List.map ~f:eval term#args) with
    | Op.Sphere (center, radius), [] ->
        fun v -> Float.(Vector3.l2_dist center v <= radius)
    | Cuboid_plane ({ rot; _ }, dim, offset), [] ->
        fun v -> Float.(offset <= Vector3.(get dim @@ inverse_rotate rot v))
    | Cuboid { rot; id }, [ xl; xh; yl; yh; zl; zh ] ->
        fun v ->
          Float.(
            xl v && (not (xh v)) && yl v && (not (yh v)) && zl v && not (zh v))
    | Union, [ f; g ] -> fun v -> f v || g v
    | Inter, [ f; g ] -> fun v -> f v && g v
    | Sub, [ f; g ] -> fun v -> f v && not (g v)
    | op, _ -> Error.(create "Unexpected op" op [%sexp_of: Op.t] |> raise)

  let rec eval_program term =
    let rec to_obj (Apply (op, args)) =
      object
        method op = op

        method args = List.map args ~f:to_obj
      end
    in
    let rec eval term = eval_open eval term in
    eval (to_obj term)

  let rec node_to_obj graph node =
    let op, args = G.children graph node |> List.hd_exn in
    object
      method op = op

      method args = List.map args ~f:(node_to_obj graph)
    end

  let rec eval_node graph node =
    let rec eval term = eval_open eval term in
    eval (node_to_obj graph node)

  let rec eval_op eval_open graph op args =
    let obj =
      object
        method op = op

        method args = List.map args ~f:(node_to_obj graph)
      end
    in
    let rec eval term = eval_open eval term in
    eval obj

  let eval_on eval points =
    List.map points ~f:(fun v -> (v, eval v)) |> Abs.of_list

  let abs_eval_open eval term =
    match (term#op, term#args) with
    | Op.Cuboid { rot; id }, [ xl; xh; yl; yh; zl; zh ] ->
        List.reduce_exn ~f:Abs.inter
          [ xl; Abs.negate xh; yl; Abs.negate yh; zl; Abs.negate zh ]
    | Union, [ x; y ] -> Abs.union x y
    | Inter, [ x; y ] -> Abs.inter x y
    | Sub, [ x; y ] -> Abs.sub x y
    | op, _ -> Error.(create "Unexpected op" op [%sexp_of: Op.t] |> raise)

  let rec abs_eval_op graph op args =
    let obj =
      object
        method op = op

        method args = List.map args ~f:(fun v -> v.State_node0.state)
      end
    in
    let rec eval term = abs_eval_open eval term in
    eval obj

  let rec size (Apply (op, args)) = 1 + List.sum (module Int) args ~f:size
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
 *     refine 1 *)

(* let refine_many () =
 *   match !refine_strategy with
 *   | `First -> refine_ordered
 *   | `Random -> refine_random ~state:(Random.State.make [||])
 *   | `Pareto -> refine_hybrid *)

let rec refine_child graph node op children =
  let open State_node0 in
  if List.is_empty children then []
  else
    let old_inputs = List.map children ~f:state in

    let strong_enough args = Abs.(node.state ==> eval' op args) in

    let new_inputs = refine_many () strong_enough old_inputs in

    (* The new state refines the old state. *)
    assert (
      List.for_all2_exn old_inputs new_inputs ~f:(fun old new_ ->
          Abs.(old ==> new_)) );

    let to_prune =
      List.zip_exn children @@ List.zip_exn old_inputs new_inputs
      |> List.filter_map ~f:(fun (node, (old, new_)) ->
             if [%compare.equal: Abs.t] old new_ then None
             else (
               set_state node new_;
               Some node ))
    in
    to_prune @ List.concat_map to_prune ~f:(refine_children graph)

and refine_children graph node =
  G.children graph node
  |> List.concat_map ~f:(fun (op, args) -> refine_child graph node op args)

(* let prune g (nodes : State_node0.t list) =
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
 *     process nodes ) *)

module Args_node = struct
  include Args_node0

  module Key = struct
    module T = struct
      type t = Op.t * State_node0.t list [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  let args_tbl = Hashtbl.create (module Key)

  let create ~op graph args =
    match Hashtbl.find args_tbl (op, args) with
    | Some v -> None
    | None ->
        let args_v = Node.Args { op; id = mk_id () } in
        List.iteri args ~f:(fun i v ->
            G.ensure_edge_e graph (args_v, i, Node.State v));
        Hashtbl.set args_tbl (op, args) args_v;
        Some args_v
end

module State_node = struct
  include State_node0

  let rec choose_program graph node =
    match G.children graph node with
    | (op, args) :: _ -> `Apply (op, List.map args ~f:(choose_program graph))
    | _ -> failwith "expected arguments"

  module Key = struct
    type t = Abs.t * Op.tag [@@deriving compare, hash, sexp]
  end

  let create ?(covered = false) ~state ~cost ~op graph children =
    match Args_node.create ~op graph children with
    | Some args_v -> (
        let tag = Op.tag op in
        match
          G.find_map_vertex graph ~f:(function
            | State v
              when [%equal: Op.tag] tag v.tag && [%equal: Abs.t] v.state state
              ->
                Some v
            | _ -> None)
        with
        | Some _ -> false
        | None ->
            let state_v =
              incr time;
              { id = mk_id (); cost; state; tag }
            in
            G.ensure_edge_e graph (Node.State state_v, -1, args_v);
            true )
    | None -> false
end

let of_cost graph tag cost =
  G.filter_map_vertex
    ~f:(function
      | State v when v.cost = cost && [%equal: Op.tag] tag v.tag -> Some v
      | _ -> None)
    graph

let fill_bool_op op graph cost =
  if cost <= 1 then false
  else
    let arg_cost = cost - 1 in
    let module Part = Combinat.Partition in
    let module Perm = Combinat.Permutation.Of_list in
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
                      let cs = of_cost graph Shape c
                      and cs' = of_cost graph Shape c' in
                      List.iteri cs ~f:(fun i (a : State_node.t) ->
                          List.iteri cs' ~f:(fun j (a' : State_node.t) ->
                              let did_add =
                                State_node.create
                                  ~state:
                                    (Program.abs_eval_op graph op [ a; a' ])
                                  ~cost
                                  ~op:(op :> Op.t)
                                  graph [ a; a' ]
                              in
                              added := !added || did_add))
                  | _ -> failwith "Unexpected costs"));
    !added

let fill_repeat op graph cost =
  if cost <= 1 then false
  else
    let arg_cost = cost - 1 in
    let module Part = Combinat.Partition in
    let module Perm = Combinat.Permutation.Of_list in
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
                      let cs = of_cost graph Count c
                      and cs' = of_cost graph Shape c' in
                      List.iteri cs ~f:(fun i (a : State_node.t) ->
                          List.iteri cs' ~f:(fun j (a' : State_node.t) ->
                              let did_add =
                                State_node.create
                                  ~state:
                                    (Program.abs_eval_op graph op [ a; a' ])
                                  ~cost
                                  ~op:(op :> Op.t)
                                  graph [ a; a' ]
                              in
                              added := !added || did_add))
                  | _ -> failwith "Unexpected costs"));
    !added

let fill_cuboid op graph cost =
  if cost <= 1 then false
  else
    let arg_cost = cost - 1 in
    let module Part = Combinat.Partition in
    let module Perm = Combinat.Permutation.Of_list in
    let added = ref false in
    Part.create ~n:arg_cost ~parts:6
    |> Part.iter ~f:(fun arg_costs ->
           let arg_costs =
             List.init (Bigarray.Array1.dim arg_costs) ~f:(fun i ->
                 arg_costs.{i})
           in
           Perm.(create arg_costs |> to_list)
           |> List.dedup_and_sort ~compare:[%compare: int list]
           |> List.iter ~f:(fun arg_costs ->
                  let rec fill_arg args = function
                    | c :: cs ->
                        of_cost graph Plane c
                        |> List.iteri ~f:(fun i (a : State_node.t) ->
                               fill_arg (args @ [ a ]) cs)
                    | [] ->
                        let did_add =
                          State_node.create
                            ~state:(Program.abs_eval_op graph op args)
                            ~cost
                            ~op:(op :> Op.t)
                            graph args
                        in
                        added := !added || did_add
                  in
                  fill_arg [] arg_costs));
    !added

let rec fill graph bench cost =
  if cost <= 1 then false
  else if fill graph bench (cost - 1) then true
  else
    List.exists ~f:Fun.id
      ( [
          fill_bool_op Union graph cost;
          fill_bool_op Inter graph cost;
          fill_bool_op Sub graph cost;
        ]
      @ List.map bench.Cad_bench.repeats ~f:(fun r ->
            fill_repeat (Repeat r) graph cost)
      @ List.mapi bench.cuboids ~f:(fun id c ->
            fill_cuboid (Cuboid { id; rot = c.rot }) graph cost) )

let refine graph (node : State_node.t) bad =
  let old = node.state in

  (* The current state should contain the bad behavior. *)
  assert (Abs.(old ==> bad));

  (* Select a point that the current state does not define and split into two
     states. *)
  let point = Set.diff (Abs.domain bad) (Abs.domain old) |> Set.choose_exn in
  let left = State_node.copy node ~state:(Abs.add node.state point true)
  and right = State_node.copy node ~state:(Abs.add node.state point false) in
  G.split_vertex graph ~orig:(State node) ~left:(State left)
    ~right:(State right);

  (* Check that at least one state excludes the bad behavior. *)
  assert (Abs.((not (left.state ==> bad)) || not (right.state ==> bad)));

  [ left; right ]

let rec strengthen graph (node : State_node.t) bad_out =
  assert (Abs.(node.state ==> bad_out));
  let to_prune = refine graph node bad_out in
  let to_prune' = List.concat_map to_prune ~f:(refine_children graph) in
  to_prune @ to_prune'

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

let synth ?(no_abstraction = false) bench =
  let graph = G.create ()
  and step = ref 0
  and expected = Abs.of_list bench.Cad_bench.io
  and inputs = List.map bench.Cad_bench.io ~f:(fun (i, _) -> i) in

  let module Viz = Graph.Graphviz.Dot (struct
    include G

    let graph_attributes _ = []

    let default_vertex_attributes _ = []

    let vertex_name n = Fmt.str "%d" @@ Node.id n

    let vertex_attributes = function
      | Node.State n ->
          let attr = [ `HtmlLabel (Fmt.str "%a" State_node.graphviz_pp n) ] in
          let attr =
            if Abs.is_subset ~of_:expected n.state then `Style `Bold :: attr
            else attr
          in
          attr
      | Args n ->
          [ `HtmlLabel (Fmt.str "%a" Args_node.graphviz_pp n); `Shape `Box ]

    let get_subgraph _ = None

    let default_edge_attributes _ = []

    let edge_attributes (_, i, _) =
      if i >= 0 then [ `Label (sprintf "%d" i) ] else []
  end) in
  let dump () =
    if !enable_dump then (
      Out_channel.with_file (sprintf "out%d.dot" !step) ~f:(fun ch ->
          Viz.output_graph ch graph);
      Out_channel.flush stdout;
      incr step )
  in

  let n_refuted = ref 0 in

  let refute () =
    match
      G.find_map_vertex graph ~f:(function
        | State v when Abs.(v.state ==> expected) -> Some v
        | _ -> None)
    with
    | Some v ->
        let output = Program.(eval_on (eval_node graph v) inputs) in
        if Abs.(expected ==> output) then raise (Done `Sat)
        else (
          incr n_refuted;
          let to_prune = strengthen graph v output in
          dump ();
          prune graph to_prune;
          dump () );
        true
    | None -> false
  in

  let rec search cost =
    if cost > !max_size then raise (Done `Unsat);
    let rec strengthen_and_cover () =
      let did_strengthen = refute () in
      if did_strengthen then strengthen_and_cover ()
    in
    strengthen_and_cover ();
    let changed = fill graph bench cost in
    Fmt.epr "Changed: %b Cost: %d\n%!" changed cost;
    if changed then dump ();
    let cost = if changed then cost else cost + 1 in
    search cost
  in

  (* Add inputs to the state space graph. *)
  let mk_state op =
    State_node.create ~state:Abs.top ~cost:1 ~op graph [] |> ignore
  in
  List.iter bench.spheres ~f:(fun s -> mk_state (Op.Sphere s));
  List.iteri bench.cuboids ~f:(fun i c ->
      let cuboid = Op.{ id = i; rot = c.rot } in
      List.iter c.x ~f:(fun x -> mk_state (Op.Cuboid_plane (cuboid, X, x)));
      List.iter c.y ~f:(fun y -> mk_state (Op.Cuboid_plane (cuboid, Y, y)));
      List.iter c.z ~f:(fun z -> mk_state (Op.Cuboid_plane (cuboid, Z, z))));
  dump ();

  try search 1
  with Done status ->
    let widths =
      G.filter_map_vertex graph ~f:(function
        | State v -> Some (Abs.width v.state)
        | _ -> None)
      |> List.sort ~compare:[%compare: int]
      |> Array.of_list
    in
    ( graph,
      Stats.
        {
          n_state_nodes =
            G.filter_vertex graph ~f:(function State v -> true | _ -> false)
            |> List.length;
          n_arg_nodes =
            G.filter_vertex graph ~f:(function Args v -> true | _ -> false)
            |> List.length;
          n_covered = -1;
          n_refuted = !n_refuted;
          min_width = widths.(0);
          max_width = widths.(Array.length widths - 1);
          median_width = widths.(Array.length widths / 2);
          sat = (match status with `Sat -> true | `Unsat -> false);
        } )

(* let sample ?(state = Random.State.default) inputs =
 *   let open Grammar in
 *   let named_inputs = List.mapi inputs ~f:(fun i x -> (sprintf "i%d" i, x)) in
 *   let input_rules =
 *     List.map named_inputs ~f:(fun (n, _) -> Rule.create "p" (Term.app n []) [])
 *   in
 *   let g =
 *     input_rules
 *     @ [
 *         Rule.create "p"
 *           (Term.app "and" [ Term.nonterm "p"; Term.nonterm "p" ])
 *           [];
 *         Rule.create "p"
 *           (Term.app "or" [ Term.nonterm "p"; Term.nonterm "p" ])
 *           [];
 *         Rule.create "p"
 *           (Term.app "diff" [ Term.nonterm "p"; Term.nonterm "p" ])
 *           [];
 *       ]
 *   in
 *   let rec to_prog = function
 *     | App (func, args) ->
 *         let op =
 *           match func with
 *           | "and" -> `Inter
 *           | "or" -> `Union
 *           | "diff" -> `Sub
 *           | _ -> (
 *               match
 *                 List.Assoc.find ~equal:[%compare.equal: string] named_inputs
 *                   func
 *               with
 *               | Some i -> `Input i
 *               | None -> failwith "unexpected function" )
 *         in
 *         let args = List.map args ~f:to_prog in
 *         `Apply (op, args)
 *     | _ -> failwith "unexpected term"
 *   in
 *   let rec sample_prog () =
 *     let p = to_prog (Grammar.sample ~state "p" g :> Untyped_term.t) in
 *     if Program.size p > !max_size then sample_prog () else p
 *   in
 *   sample_prog ()
 * 
 * let check_search_space ?(n = 100_000) inputs graph =
 *   let rec loop i =
 *     if i > n then Ok ()
 *     else
 *       let prog = sample inputs in
 *       let cstate = Program.ceval prog in
 *       match
 *         G.find_map_vertex graph ~f:(function
 *           | State v when Abs.contains v.state cstate -> Some v
 *           | _ -> None)
 *       with
 *       | Some v -> loop (i + 1)
 *       | None ->
 *           Fmt.epr "Missed program %a with size %d and state %a\n" Sexp.pp
 *             ([%sexp_of: Program.t] prog)
 *             (Program.size prog) Conc.pp cstate;
 *           Error cstate
 *   in
 *   loop 0
 * 
 * let random_likely_unsat ?(state = Random.State.default) n k =
 *   let inputs =
 *     List.init n ~f:(fun _ -> Array.init k ~f:(fun _ -> Random.State.bool state))
 *   in
 *   let output = Array.init k ~f:(fun _ -> Random.State.bool state) in
 *   (inputs, output)
 * 
 * let random_sat ?(state = Random.State.default) n k =
 *   let inputs =
 *     List.init n ~f:(fun _ -> Array.init k ~f:(fun _ -> Random.State.bool state))
 *   in
 *   let output = sample ~state inputs |> Program.ceval in
 *   (inputs, output)
 * 
 * let random_io ?(state = Random.State.default) ~n ~k =
 *   (\* if Random.State.bool state then random_sat ~state n k
 *    * else *\)
 *   random_likely_unsat ~state n k *)
