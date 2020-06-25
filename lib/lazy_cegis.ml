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
    type t = bool array [@@deriving compare, sexp]

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
  type t [@@deriving compare, sexp]

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

module Map_abs : ABS = struct
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

  (* let is_subset_a =
   *   let hashable =
   *     let module V = struct
   *       type nonrec t = t * t [@@deriving compare, hash, sexp]
   *     end in
   *     Base.Hashable.of_key (module V)
   *   in
   *   let func = Memo.general ~hashable (fun (s, s') -> is_subset_a s ~of_:s') in
   *   fun s ~of_:s' -> func (s, s')
   * 
   * (\* Map.for_alli s ~f:(fun ~key ~data:x ->
   *  *     match Map.find s' key with Some x' -> Bool.(x = x') | None -> false) *\) *)

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

module Array_abs : ABS = struct
  type t = int array [@@deriving compare, sexp]

  let top = [||]

  let pp =
    Fmt.using (fun m ->
        Array.filter_mapi m ~f:(fun i x -> if x >= 0 then Some (x, i) else None))
    @@ Fmt.array ~sep:(Fmt.any " ")
    @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "_%d")

  let graphviz_pp =
    Fmt.using (fun m ->
        Array.filter_mapi m ~f:(fun i x -> if x >= 0 then Some (x, i) else None))
    @@ Fmt.array ~sep:(Fmt.any " ")
    @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "<sub>%d</sub>")

  let get a i = if i >= Array.length a then -1 else a.(i) [@@inline always]

  let init a i = if i >= Array.length a then -1 else a.(i)

  let map2 a a' ~f =
    Array.init
      (Int.max (Array.length a) (Array.length a'))
      ~f:(fun i -> f (get a i) (get a' i))

  let for_all2 a a' ~f =
    let rec for_all i =
      if i >= Array.length a || i >= Array.length a' then true
      else if f (get a i) (get a' i) then for_all (i + 1)
      else false
    in
    for_all 0

  let meet v v' =
    map2 v v' ~f:(fun x x' ->
        if x < 0 then x'
        else if x' < 0 then x
        else (
          assert (x = x');
          x ))

  let is_subset_a s ~of_:s' =
    for_all2 s s' ~f:(fun x x' -> if x' >= 0 then x = x' else false)

  let lift s = Array.mapi s ~f:(fun i x -> if x then 1 else 0)

  let union v v' =
    map2 v v' ~f:(fun x x' ->
        if x >= 0 && x' >= 0 then Int.min 1 (x + x')
        else if x = 1 || x' = 1 then 1
        else -1)

  let inter v v' =
    map2 v v' ~f:(fun x x' ->
        if x >= 0 && x' >= 0 then Int.max 0 (x + x' - 1)
        else if x = 0 || x' = 0 then 0
        else -1)

  let sub v v' =
    map2 v v' ~f:(fun x x' ->
        if x >= 0 && x' >= 0 then Int.max 0 (x - x')
        else if x = 0 || x' = 1 then 0
        else -1)

  let mem v i = get v i >= 0

  let add_exn m k v =
    let m' = Array.create ~len:(Int.max (Array.length m) (k + 1)) (-1) in
    Array.blit ~src:m ~src_pos:0 ~dst:m' ~dst_pos:0 ~len:(Array.length m);
    m'.(k) <- (if v then 1 else 0);
    m'

  let contains a c =
    Array.for_alli c ~f:(fun i x -> get a i < 0 || Bool.to_int x = get a i)

  let width = Array.length

  let of_list_exn l =
    List.fold_left l ~init:top ~f:(fun acc (i, x) -> add_exn acc i x)
end

module Abs = Map_abs

module Op = struct
  type t = [ `Input of Conc.t | `Union | `Inter | `Sub ]
  [@@deriving compare, hash, sexp]

  let pp fmt op =
    let str =
      match op with
      | `Input _ -> "in"
      | `Union -> "or"
      | `Inter -> "and"
      | `Sub -> "diff"
    in
    Fmt.pf fmt "%s" str
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

  type t = {
    id : int;
    cstate : Conc.t;
    mutable cost : int;
    mutable state : Abs.t;
    mutable covered : bool;
    mutable last_mod_time : int;
  }
  [@@deriving sexp]

  let id { id; _ } = id

  let state { state; _ } = state

  let cstate { cstate; _ } = cstate

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

  let graphviz_pp fmt { state; cost; _ } =
    Fmt.pf fmt "%a %d" Abs.graphviz_pp state cost

  let hash n = [%hash: int] n.id

  let hash_fold_t state n = [%hash_fold: int] state n.id

  let equal n n' = [%compare.equal: int] n.id n'.id

  let compare n n' = [%compare: int] n.id n'.id
end

module Node = struct
  type t = Args of Args_node0.t | State of State_node0.t
  [@@deriving compare, hash]

  let equal = [%compare.equal: t]

  let id = function Args x -> Args_node0.id x | State x -> State_node0.id x

  let to_args = function Args x -> x | _ -> failwith "expected an args node"
end

let contains a c = Map.for_alli a ~f:(fun ~key:i ~data:v -> Bool.(c.(i) = v))

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
end

let ceval' op args =
  match (op, args) with
  | `Input x, _ -> x
  | `Union, [ x; y ] -> Array.map2_exn x.State_node0.cstate y.cstate ~f:( || )
  | `Inter, [ x; y ] -> Array.map2_exn x.cstate y.cstate ~f:( && )
  | `Sub, [ x; y ] ->
      Array.map2_exn x.cstate y.cstate ~f:(fun a b -> a && not b)
  | _ -> assert false

let eval' op args =
  match (op, args) with
  | `Input state, _ -> Abs.lift state
  | `Union, [ x; y ] -> Abs.union x y
  | `Inter, [ x; y ] -> Abs.inter x y
  | `Sub, [ x; y ] -> Abs.sub x y
  | _ -> failwith "Unexpected args"

let eval g a =
  let args =
    G.succ_e g (Node.Args a)
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
    | `Input x, _ -> x
    | `Union, [ x; y ] -> Array.map2_exn (ceval x) (ceval y) ~f:( || )
    | `Inter, [ x; y ] -> Array.map2_exn (ceval x) (ceval y) ~f:( && )
    | `Sub, [ x; y ] ->
        Array.map2_exn (ceval x) (ceval y) ~f:(fun a b -> a && not b)
    | _ -> assert false

  let rec size (`Apply (op, args)) = 1 + List.sum (module Int) args ~f:size

  include T
  include Comparator.Make (T)
end

let refine_ordered strong_enough abs conc =
  let open State_node0 in
  let abs = Array.of_list abs and conc = Array.of_list conc in
  let n = Array.length conc and k = Array.length conc.(0) in
  let rec loop i j =
    if i >= n || j >= k then failwith "Could not refine";

    if not (Abs.mem abs.(i) j) then
      abs.(i) <- Abs.add_exn abs.(i) j conc.(i).(j);

    if not (strong_enough @@ Array.to_list abs) then
      if j = k - 1 then loop (i + 1) 0 else loop i (j + 1)
  in
  loop 0 0;
  Array.to_list abs

let refine_random ?(state = Random.State.default) strong_enough abs conc =
  if strong_enough abs then abs
  else
    let abs = Array.of_list abs and conc = Array.of_list conc in
    let n = Array.length conc and k = Array.length conc.(0) in
    let choices =
      List.init n ~f:(fun i ->
          List.init k ~f:(fun j ->
              if Abs.mem abs.(i) j then None else Some (i, j))
          |> List.filter_map ~f:Fun.id)
      |> List.concat
      |> List.permute ~random_state:state
      |> Queue.of_list
    in
    let rec refine () =
      let i, j =
        match Queue.dequeue choices with
        | Some x -> x
        | None -> failwith Fmt.(str "cannot refine %a" (Dump.array Abs.pp) abs)
      in
      abs.(i) <- Abs.add_exn abs.(i) j conc.(i).(j);
      let abs_list = Array.to_list abs in
      if strong_enough abs_list then abs_list else refine ()
    in
    refine ()

let refine_pareto strong_enough abs conc =
  if strong_enough abs then abs
  else
    let abs_a = Array.of_list abs and conc = Array.of_list conc in
    let n = Array.length conc and k = Array.length conc.(0) in
    let choices =
      List.init n ~f:(fun i ->
          List.init k ~f:(fun j ->
              if Abs.mem abs_a.(i) j then None else Some (i, j))
          |> List.filter_map ~f:Fun.id)
      |> List.concat
    in
    let rec refine n_choices =
      if n_choices > List.length choices then failwith "cannot refine";
      match
        Combinat.Combination.Of_list.(
          create choices n_choices
          |> find_map ~f:(fun cs ->
                 let abs = Array.of_list abs in
                 List.iter cs ~f:(fun (i, j) ->
                     abs.(i) <- Abs.add_exn abs.(i) j conc.(i).(j));
                 let abs = Array.to_list abs in
                 if strong_enough abs then Some abs else None))
      with
      | Some abs -> abs
      | None -> refine (n_choices + 1)
    in
    refine 1

let refine_hybrid strong_enough abs conc =
  if strong_enough abs then abs
  else
    let abs_a = Array.of_list abs and conc_a = Array.of_list conc in
    let n = Array.length conc_a and k = Array.length conc_a.(0) in
    let choices =
      List.init n ~f:(fun i ->
          List.init k ~f:(fun j ->
              if Abs.mem abs_a.(i) j then None else Some (i, j))
          |> List.filter_map ~f:Fun.id)
      |> List.concat
    in
    let rec refine n_choices =
      if n_choices > 4 then refine_ordered strong_enough abs conc
      else
        match
          Combinat.Combination.Of_list.(
            create choices n_choices
            |> find_map ~f:(fun cs ->
                   let abs = Array.of_list abs in
                   List.iter cs ~f:(fun (i, j) ->
                       abs.(i) <- Abs.add_exn abs.(i) j conc_a.(i).(j));
                   let abs = Array.to_list abs in
                   if strong_enough abs then Some abs else None))
        with
        | Some abs -> abs
        | None -> refine (n_choices + 1)
    in
    refine 1

let refine_many () =
  match !refine_strategy with
  | `First -> refine_ordered
  | `Random -> refine_random ~state:(Random.State.make [||])
  | `Pareto -> refine_hybrid

let rec refine_child graph node op children =
  let open State_node0 in
  if List.is_empty children then []
  else
    let conc_inputs = List.map children ~f:cstate
    and old_inputs = List.map children ~f:state in

    (* The old state contains the concrete behavior. *)
    assert (
      List.zip_exn old_inputs conc_inputs
      |> List.for_all ~f:(fun (old, conc) -> Abs.contains old conc) );

    let strong_enough args = Abs.is_subset_a node.state ~of_:(eval' op args) in

    let new_inputs = refine_many () strong_enough old_inputs conc_inputs in

    (* The new state refines the old state. *)
    assert (
      List.zip_exn old_inputs new_inputs
      |> List.for_all ~f:(fun (old, new_) -> Abs.is_subset_a old ~of_:new_) );

    (* The new state contains the concrete behavior. *)
    assert (
      List.zip_exn conc_inputs new_inputs
      |> List.for_all ~f:(fun (conc, new_) -> Abs.contains new_ conc) );

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

let prune g (nodes : State_node0.t list) =
  let open State_node0 in
  if not (List.is_empty nodes) then (
    let min_mod_time =
      Option.value_exn
        ( List.map nodes ~f:(fun n -> n.last_mod_time)
        |> List.min_elt ~compare:[%compare: int] )
    in
    G.iter_state_vertex
      ~f:(fun v -> if v.last_mod_time > min_mod_time then uncover v)
      g;
    Fmt.epr "Prune: min mod time %d.\n" min_mod_time;

    let rec process = function
      | v :: vs ->
          let work =
            (* Select the arg nodes that depend on this state. *)
            G.pred g (Node.State v)
            |> List.concat_map ~f:(function
                 | Node.Args a ->
                     let state = eval g a in

                     (* Push the state update forward through the args nodes. *)
                     List.concat_map (G.pred g (Args a)) ~f:(function
                       | State v' ->
                           let old = v'.state in
                           let new_ = Abs.meet old state in
                           if [%compare.equal: Abs.t] old new_ then []
                           else (
                             set_state v' new_;
                             v' :: refine_children g v' )
                       | Args _ -> failwith "expected a state node")
                 | State _ -> failwith "expected an args node")
          in
          process (work @ vs)
      | [] -> ()
    in
    process nodes )

(* module Cover_table = struct
 *   module Key = struct
 *     include Int
 * 
 *     let create pos value = if value then pos else -pos
 *   end
 * 
 *   type t = State_node0.t list Hashtbl.M(Key).t
 * 
 *   let 
 * end *)

let update_covers graph =
  let uncovered =
    G.filter_map_vertex graph ~f:(function
      | State v when not v.covered -> Some v
      | _ -> None)
  in
  List.iter uncovered ~f:(fun v ->
      if
        List.exists uncovered ~f:(fun v' ->
            (not v'.covered) && v'.cost <= v.cost
            && (not ([%compare.equal: State_node0.t] v v'))
            && Abs.is_subset_a v'.state ~of_:v.state)
      then State_node0.cover v)

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

  (* match
   *   G.find_vertex graph ~f:(function
   *     | Args v ->
   *         [%compare.equal: Op.t] v.op op
   *         && List.for_alli args ~f:(fun i state ->
   *                G.mem_edge_e graph (Node.Args v, i, Node.State state))
   *     | _ -> false)
   * with
   * | Some v -> v
   * | None ->
   *     let args_v = Node.Args { op; id = mk_id () } in
   *     List.iteri args ~f:(fun i v ->
   *         G.ensure_edge_e graph (args_v, i, Node.State v));
   *     args_v *)
end

module State_node = struct
  include State_node0

  let state_tbl = Hashtbl.create (module Conc)

  let rec choose_program graph node =
    match G.children graph node with
    | (op, args) :: _ -> `Apply (op, List.map args ~f:(choose_program graph))
    | _ -> failwith "expected arguments"

  let create ?(covered = false) ~state ~cost ~op graph children =
    match Args_node.create ~op graph children with
    | Some args_v ->
        let cstate = ceval' op children in
        let state_v, did_add =
          match Hashtbl.find state_tbl cstate with
          | Some v ->
              v.cost <- Int.min v.cost cost;
              refine_child graph v op children |> prune graph;
              update_covers graph;
              (v, true)
          | None ->
              incr time;
              let v =
                {
                  id = mk_id ();
                  cost;
                  state;
                  cstate;
                  covered;
                  last_mod_time = !time;
                }
              in
              Hashtbl.add_exn state_tbl cstate v;
              (v, true)
        in
        G.ensure_edge_e graph (Node.State state_v, -1, args_v);
        did_add
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
      G.filter_map_vertex
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
                      let cs = of_cost c and cs' = of_cost c' in
                      let total = List.length cs * List.length cs' * 3 in
                      let print_percent i j k =
                        Fmt.epr "Fill %d%%\r%!"
                          ( ((i * (List.length cs' * 3)) + (j * 3) + k)
                          * 100 / total )
                      in
                      List.iteri cs ~f:(fun i (a : State_node.t) ->
                          List.iteri cs' ~f:(fun j (a' : State_node.t) ->
                              List.iteri [ `Union; `Inter; `Sub ]
                                ~f:(fun k op ->
                                  print_percent i j k;
                                  let state =
                                    match op with
                                    | `Union -> Abs.union a.state a'.state
                                    | `Inter -> Abs.inter a.state a'.state
                                    | `Sub -> Abs.sub a.state a'.state
                                  in
                                  let did_add =
                                    State_node.create ~state ~cost
                                      ~op:(op :> Op.t)
                                      graph [ a; a' ]
                                  in
                                  added := !added || did_add)))
                  | _ -> failwith "Unexpected costs"));
    !added

let refine graph (node : State_node.t) bad =
  let conc = node.cstate and old = node.state in

  (* The bad behavior should not be the same as the concrete behavior, the
     initial state should abstract the bad behavior and the concrete behavior.
  *)
  assert (
    (not ([%compare.equal: Conc.t] bad conc))
    && Abs.contains old bad && Abs.contains old conc );

  let len = Array.length conc in
  let rec loop i =
    if i >= len then failwith "Could not refine"
    else if Bool.(conc.(i) <> bad.(i)) then Abs.add_exn node.state i conc.(i)
    else loop (i + 1)
  in
  let new_ = loop 0 in

  (* Check that the new state refines the old one, does not contain the bad
     state, and still abstracts the concrete behavior. *)
  assert (Abs.is_subset_a old ~of_:new_);
  assert (Abs.(not (contains new_ bad)));
  assert (Abs.contains new_ conc);

  State_node.set_state node new_;
  [ node ]

let rec strengthen graph (node : State_node.t) bad_out =
  assert (Abs.contains node.state bad_out);
  let to_prune = refine graph node bad_out in
  let to_prune' = refine_children graph node in
  assert (not (Abs.contains node.state bad_out));
  to_prune @ to_prune'

let%expect_test "" =
  let g = G.create () in
  State_node.create
    ~state:(Abs.of_list_exn [ (0, true) ])
    ~op:(`Input [| true; true |])
    ~cost:1 g []
  |> ignore;
  State_node.create
    ~state:(Abs.of_list_exn [ (1, true) ])
    ~op:(`Input [| true; false |])
    ~cost:1 g []
  |> ignore;
  fill g 3 |> ignore;
  G.output_graph stdout g;
  [%expect
    {|
    digraph G {
      0 [label=<in 1_0>, ];
      1 [label=<in 1_1>, ];
      2 [label=<or 1_1>, ];
      3 [label=<and 1_1>, ];
      4 [label=<diff 0_1>, ];
      5 [label=<or 1_0 1_1>, ];
      6 [label=<and >, ];
      7 [label=<diff 0_0>, ];
      8 [label=<or 1_0 1_1>, ];
      9 [label=<and >, ];
      10 [label=<diff 0_1>, ];
      11 [label=<or 1_0>, ];
      12 [label=<and 1_0>, ];
      13 [label=<diff 0_0>, ];


      2 -> 1 [label="0", ];
      2 -> 1 [label="1", ];
      3 -> 1 [label="0", ];
      3 -> 1 [label="1", ];
      4 -> 1 [label="0", ];
      4 -> 1 [label="1", ];
      5 -> 0 [label="1", ];
      5 -> 1 [label="0", ];
      6 -> 0 [label="1", ];
      6 -> 1 [label="0", ];
      7 -> 0 [label="1", ];
      7 -> 1 [label="0", ];
      8 -> 0 [label="0", ];
      8 -> 1 [label="1", ];
      9 -> 0 [label="0", ];
      9 -> 1 [label="1", ];
      10 -> 0 [label="0", ];
      10 -> 1 [label="1", ];
      11 -> 0 [label="0", ];
      11 -> 0 [label="1", ];
      12 -> 0 [label="0", ];
      12 -> 0 [label="1", ];
      13 -> 0 [label="0", ];
      13 -> 0 [label="1", ];

      } |}]

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

let synth ?(no_abstraction = false) inputs output =
  let graph = G.create () and step = ref 0 in

  let module Viz = Graph.Graphviz.Dot (struct
    include G

    let graph_attributes _ = []

    let default_vertex_attributes _ = []

    let vertex_name n = Fmt.str "%d" @@ Node.id n

    let vertex_attributes = function
      | Node.State n ->
          [ `HtmlLabel (Fmt.str "%a" State_node.graphviz_pp n) ]
          @
          if n.covered then [ `Style `Dotted ]
          else [] @ if Abs.contains n.state output then [ `Style `Bold ] else []
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
        | State v when (not v.covered) && Abs.contains v.state output -> Some v
        | _ -> None)
    with
    | Some v ->
        if [%compare.equal: Conc.t] v.cstate output then raise (Done `Sat)
        else (
          incr n_refuted;
          (* Fmt.epr "Refuting: %a\n" Sexp.pp_hum
           *   ([%sexp_of: Program.t] (State_node.choose_program graph v)); *)
          let to_prune = strengthen graph v output in
          dump ();
          prune graph to_prune;
          dump () );
        true
    | None -> false
  in

  let rec loop cost =
    if cost > !max_size then raise (Done `Unsat);
    let rec strengthen_and_cover () =
      update_covers graph;
      dump ();
      let did_strengthen = refute () in
      if did_strengthen then strengthen_and_cover ()
    in
    strengthen_and_cover ();

    let changed = fill graph cost in
    Fmt.epr "Changed: %b Cost: %d\n%!" changed cost;
    if changed then dump ();
    let cost = if changed then cost else cost + 1 in
    loop cost
  in

  (* Add inputs to the state space graph. *)
  List.iter inputs ~f:(fun input ->
      let state = if no_abstraction then Abs.lift input else Abs.top in
      State_node.create ~state ~cost:1 ~op:(`Input input) graph [] |> ignore);
  dump ();

  try loop 1
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
          n_covered =
            G.filter_vertex graph ~f:(function
              | State v -> v.covered
              | _ -> false)
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
          | "and" -> `Inter
          | "or" -> `Union
          | "diff" -> `Sub
          | _ -> (
              match
                List.Assoc.find ~equal:[%compare.equal: string] named_inputs
                  func
              with
              | Some i -> `Input i
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
        G.find_map_vertex graph ~f:(function
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
