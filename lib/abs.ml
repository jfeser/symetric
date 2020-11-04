open Ast

module Bool_vector = struct
  module T = struct
    type t = bool Map.M(Int).t [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let top = Map.empty (module Int)

  let pp : t Fmt.t =
    Fmt.using (fun m ->
        Map.to_alist m |> List.map ~f:(fun (k, v) -> (Bool.to_int v, k)))
    @@ Fmt.list ~sep:(Fmt.any " ")
    @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "_%d")

  let graphviz_pp : t Fmt.t =
    Fmt.using (fun m ->
        Map.to_alist m |> List.map ~f:(fun (k, v) -> (Bool.to_int v, k)))
    @@ Fmt.list ~sep:(Fmt.any " ")
    @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "<sub>%d</sub>")

  let meet =
    Map.merge ~f:(fun ~key:_ -> function
      | `Left x | `Right x -> Some x | `Both _ -> None)

  let is_subset_a s ~of_:s' =
    if Map.length s > Map.length s' then false
    else
      Map.fold2 s s' ~init:true ~f:(fun ~key:_ ~data acc ->
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
    Map.merge ~f:(fun ~key:_ -> function
      | `Both (x, x') -> Some (x || x')
      | `Left true | `Right true -> Some true
      | `Left false | `Right false -> None)

  let inter =
    Map.merge ~f:(fun ~key:_ -> function
      | `Both (x, x') -> Some (x && x')
      | `Left false | `Right false -> Some false
      | `Left true | `Right true -> None)

  let sub =
    Map.merge ~f:(fun ~key:_ -> function
      | `Both (x, x') -> Some (x && not x')
      | `Left false | `Right true -> Some false
      | `Left true | `Right false -> None)

  let mem v i = Map.mem v i

  let set m k v = Map.set m ~key:k ~data:v

  let add m k v =
    match Map.find m k with
    | Some v' -> if Bool.(v = v') then Some m else None
    | None -> Some (set m k v)

  let add_exn m k v = Option.value_exn (add m k v)

  let contains a c = Map.for_alli a ~f:(fun ~key:i ~data:v -> Bool.(c.(i) = v))

  let width = Map.length

  let of_list_exn l = Map.of_alist_exn (module Int) l
end

module Offset = struct
  type t = { lo : float; hi : float } [@@deriving compare, hash, sexp]

  let contains a c = Float.(a.lo <= c && c <= a.hi)

  let graphviz_pp fmt x = Fmt.pf fmt "[%f, %f]" x.lo x.hi

  let top = { lo = Float.min_value; hi = Float.max_value }

  let lift x = { lo = x; hi = x }
end

module T = struct
  type t = Bool_vector of Bool_vector.t | Offset of Offset.t
  [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

let bool_vector x = Bool_vector x

let offset x = Offset x

let map ~bool_vector ~offset = function
  | Bool_vector x -> bool_vector x
  | Offset x -> offset x

let top = function
  | Type.Vector -> bool_vector @@ Bool_vector.top
  | Offset _ -> offset Offset.top

let graphviz_pp fmt =
  map
    ~bool_vector:(Bool_vector.graphviz_pp fmt)
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
  | Bool_vector v, Conc.Bool_vector v' -> Bool_vector.contains v v'
  | Offset v, Conc.Offset v' -> Offset.contains v v'
  | _ -> false

let lift = function
  | Conc.Bool_vector x -> bool_vector @@ Bool_vector.lift x
  | Offset x -> offset @@ Offset.lift x

let cylinder (c : Op.cylinder) l h =
  let l = to_offset_exn l and h = to_offset_exn h in
  let open Sequence in
  (Set_once.get_exn Global.bench [%here]).input |> Array.to_sequence
  |> filter_mapi ~f:(fun i v ->
         let open Vector3 in
         let rot = inverse_rotate v c.theta in
         let in_radius =
           Float.(square (rot.y - c.y) + square (rot.z - c.z) < square c.radius)
         in
         let below_lo = Float.(rot.x < l.lo)
         and above_hi = Float.(rot.x > h.hi)
         and above_lo = Float.(rot.x >= l.hi)
         and below_hi = Float.(rot.x <= h.lo) in
         let is_out = (not in_radius) || below_lo || above_hi
         and is_in = in_radius && above_lo && below_hi in
         if is_out then Some (i, false)
         else if is_in then Some (i, true)
         else None)
  |> Map.of_sequence_exn (module Int)
  |> bool_vector

let cuboid (c : Op.cuboid) lx hx ly hy lz hz =
  let lx = to_offset_exn lx
  and hx = to_offset_exn hx
  and ly = to_offset_exn ly
  and hy = to_offset_exn hy
  and lz = to_offset_exn lz
  and hz = to_offset_exn hz in
  let open Sequence in
  (Set_once.get_exn Global.bench [%here]).input |> Array.to_sequence
  |> filter_mapi ~f:(fun i v ->
         let open Vector3 in
         let rot = inverse_rotate v c.theta in
         let below_lox = Float.(rot.x < lx.lo)
         and above_hix = Float.(rot.x > hx.hi)
         and above_lox = Float.(rot.x >= lx.hi)
         and below_hix = Float.(rot.x <= hx.lo)
         and below_loy = Float.(rot.y < ly.lo)
         and above_hiy = Float.(rot.y > hy.hi)
         and above_loy = Float.(rot.y >= ly.hi)
         and below_hiy = Float.(rot.y <= hy.lo)
         and below_loz = Float.(rot.z < lz.lo)
         and above_hiz = Float.(rot.z > hz.hi)
         and above_loz = Float.(rot.z >= lz.hi)
         and below_hiz = Float.(rot.z <= hz.lo) in
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
  |> Map.of_sequence_exn (module Int)
  |> bool_vector

let eval op args =
  let open Util in
  match op with
  | Op.Union -> apply2 union args
  | Inter -> apply2 inter args
  | Sub -> apply2 sub args
  | Cylinder c -> apply2 (cylinder c) args
  | Cuboid c -> apply6 (cuboid c) args
  | Sphere _ | Offset _ -> failwith "leaf node"
