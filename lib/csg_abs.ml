open Params

module Offset = struct
  type t = { lo : float; hi : float; type_ : Csg_type.Offset.t }
  [@@deriving compare, hash, sexp]

  type concrete = Offset.t

  let contains a c = Float.(a.lo <= Offset.offset c && Offset.offset c <= a.hi)

  let graphviz_pp fmt x = Fmt.pf fmt "[%f, %f)" x.lo x.hi

  let top ctx t =
    let min =
      Option.value_exn
        ( Offset.of_type ctx t
        |> Sequence.map ~f:Offset.offset
        |> Sequence.min_elt ~compare:[%compare: float] )
    in
    let max =
      Option.value_exn
        ( Offset.of_type ctx t
        |> Sequence.map ~f:Offset.offset
        |> Sequence.max_elt ~compare:[%compare: float] )
    in
    { lo = min; hi = max; type_ = t }

  let is_bottom x = Float.(x.lo > x.hi)

  let lift x =
    { lo = Offset.offset x; hi = Offset.offset x; type_ = Offset.type_ x }

  let create ~lo ~hi ~type_ =
    if Float.(hi < lo) then None else Some { lo; hi; type_ }

  let create_exn ~lo ~hi ~type_ = Option.value_exn (create ~lo ~hi ~type_)

  let to_symb offsets abs =
    let open Smt.Let_syntax in
    let open Symb0.Bool_vector in
    let%bind set =
      Offset.of_type offsets abs.type_
      |> Sequence.map ~f:(fun conc ->
             if not (contains abs conc) then return @@ fixed false
             else Smt.fresh_decl () >>| free)
      |> Sequence.to_list |> Smt.all
    in
    let%map () = Smt.assert_exactly_one @@ List.map ~f:to_expr set in
    Symb0.Offset.{ set; type_ = abs.type_ }

  let copy ?lo ?hi x =
    create
      ~lo:(Option.value lo ~default:x.lo)
      ~hi:(Option.value hi ~default:x.hi)
      ~type_:x.type_

  let lo x = x.lo

  let hi x = x.hi

  let split_exn x o =
    if not (contains x o) then
      raise_s [%message "range does not contain point" (x : t) (o : Offset.t)];
    [
      Option.bind (Offset.prev o) ~f:(fun o' -> copy x ~hi:(Offset.offset o'));
      create ~type_:x.type_ ~lo:(Offset.offset o) ~hi:(Offset.offset o);
      Option.bind (Offset.next o) ~f:(fun o' -> copy x ~lo:(Offset.offset o'));
    ]
    |> List.filter_map ~f:Fun.id

  let exclude_exn x o =
    split_exn x o |> List.filter ~f:(fun x' -> not (contains x' o))

  let is_subset x ~of_:x' =
    [%compare.equal: Csg_type.Offset.t] x.type_ x'.type_
    && Float.(x.lo >= x'.lo && x.hi <= x'.hi)

  let type_ x = x.type_

  let meet x x' =
    [%test_result: Csg_type.Offset.t] ~expect:x.type_ x'.type_;
    { x with lo = Float.max x.lo x'.lo; hi = Float.min x.hi x'.hi }
end

module Bool_vector = Bool_vector_precise

module T = struct
  type t = Bool_vector of Bool_vector.t | Offset of Offset.t
  [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

let bool_vector x = Bool_vector x

let offset x = Offset x

let bind ~bool_vector ~offset = function
  | Bool_vector x -> bool_vector x
  | Offset x -> offset x

let top params = function
  | Csg_type.Vector -> bool_vector @@ Bool_vector.top
  | Offset t -> offset @@ Offset.top (Csg_bench.offsets params.bench) t

let graphviz_pp params fmt =
  bind
    ~bool_vector:(Bool_vector.graphviz_pp params fmt)
    ~offset:(Offset.graphviz_pp fmt)

let to_bool_vector_exn = function
  | Bool_vector x -> x
  | v -> raise_s [%message "Expected a bool vector" (v : t)]

let to_offset_exn = function
  | Offset x -> x
  | v -> raise_s [%message "Expected an offset" (v : t)]

let union x x' =
  bool_vector
  @@ Bool_vector.union (to_bool_vector_exn x) (to_bool_vector_exn x')

let inter x x' =
  bool_vector
  @@ Bool_vector.inter (to_bool_vector_exn x) (to_bool_vector_exn x')

let sub x x' =
  bool_vector @@ Bool_vector.sub (to_bool_vector_exn x) (to_bool_vector_exn x')

let contains a c =
  match (a, c) with
  | Bool_vector v, Csg_conc.Bool_vector v' -> Bool_vector.contains v v'
  | Offset v, Offset v' -> Offset.contains v v'
  | _ -> false

let is_subset a ~of_:a' =
  match (a, a') with
  | Bool_vector x, Bool_vector x' -> Bool_vector.is_subset x ~of_:x'
  | Offset x, Offset x' -> Offset.is_subset x ~of_:x'
  | _ -> false

let meet a a' =
  match (a, a') with
  | Bool_vector a, Bool_vector a' -> bool_vector @@ Bool_vector.meet a a'
  | Offset a, Offset a' -> offset @@ Offset.meet a a'
  | _ -> raise_s [%message "unexpected inputs" (a : t) (a' : t)]

let is_bottom a =
  bind a ~bool_vector:Bool_vector.is_bottom ~offset:Offset.is_bottom

let lift = function
  | Csg_conc.Bool_vector x -> bool_vector @@ Bool_vector.lift x
  | Offset x -> offset @@ Offset.lift x

let bool_vector_of_list =
  List.fold ~init:Bool_vector.top ~f:(fun x (k, v) -> Bool_vector.add x k v)

let cylinder (c : Csg_op.cylinder) input (l : Offset.t) (h : Offset.t) =
  Array.to_list input
  |> List.filter_mapi ~f:(fun i v ->
         let open Vector3 in
         let rot = inverse_rotate v ~theta:c.theta in
         let in_radius =
           Float.(square (rot.y - c.y) + square (rot.z - c.z) < square c.radius)
         in
         let below_lo = Float.(rot.x < l.lo)
         and above_hi = Float.(rot.x >= h.hi)
         and above_lo = Float.(rot.x >= l.hi)
         and below_hi = Float.(rot.x < h.lo) in
         let is_out = (not in_radius) || below_lo || above_hi
         and is_in = in_radius && above_lo && below_hi in
         if is_out then Some (i, false)
         else if is_in then Some (i, true)
         else None)
  |> bool_vector_of_list

let cuboid (c : Csg_op.cuboid) input (lx : Offset.t) (hx : Offset.t)
    (ly : Offset.t) (hy : Offset.t) (lz : Offset.t) (hz : Offset.t) =
  Array.to_list input
  |> List.filter_mapi ~f:(fun i v ->
         let open Vector3 in
         let rot = inverse_rotate v ~theta:c.theta in
         let below_lox = Float.(rot.x < lx.lo)
         and above_lox = Float.(rot.x >= lx.hi)
         and below_hix = Float.(rot.x < hx.lo)
         and above_hix = Float.(rot.x >= hx.hi)
         and below_loy = Float.(rot.y < ly.lo)
         and above_hiy = Float.(rot.y >= hy.hi)
         and below_hiy = Float.(rot.y < hy.lo)
         and above_loy = Float.(rot.y >= ly.hi)
         and below_loz = Float.(rot.z < lz.lo)
         and above_hiz = Float.(rot.z >= hz.hi)
         and below_hiz = Float.(rot.z < hz.lo)
         and above_loz = Float.(rot.z >= lz.hi) in
         let is_out =
           below_lox || above_hix || below_loy || above_hiy || below_loz
           || above_hiz
         and is_in =
           above_lox && below_hix && above_loy && below_hiy && above_loz
           && below_hiz
         in
         if is_out then Some (i, false)
         else if is_in then Some (i, true)
         else None)
  |> bool_vector_of_list

let eval params op args =
  let open Apply in
  let eval_offsets = List.map ~f:to_offset_exn in
  match op with
  | Csg_op.Union -> apply2 union args
  | Inter -> apply2 inter args
  | Sub -> apply2 sub args
  | Cylinder c ->
      apply2 (cylinder c params.bench.Csg_bench.input) @@ eval_offsets args
      |> bool_vector
  | Cuboid c ->
      apply6 (cuboid c params.bench.input) @@ eval_offsets args |> bool_vector
  | Sphere _ | Offset _ ->
      raise_s [%message "leaf node" (op : Csg_op.t) (args : t list)]

let to_symb params =
  let open Smt.Let_syntax in
  function
  | Bool_vector x ->
      let%map s = Bool_vector.to_symb (Csg_bench.n_bits params.bench) x in
      Symb0.Bool_vector s
  | Offset x ->
      let%map s = Offset.to_symb (Csg_bench.offsets params.bench) x in
      Symb0.Offset s

let roots =
  List.fold_left ~init:[] ~f:(fun roots v ->
      match List.find roots ~f:(fun v' -> is_subset v ~of_:v') with
      | Some _ -> roots
      | None -> v :: List.filter roots ~f:(fun v' -> not (is_subset v' ~of_:v)))