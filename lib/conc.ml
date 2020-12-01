open Ast
open Params

module Bool_vector = struct
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

module Offset = Offset

type t = Bool_vector of Bool_vector.t | Offset of Offset.t
[@@deriving compare, hash, sexp]

let bool_vector x = Bool_vector x

let offset x = Offset x

let to_bool_vector_exn = function
  | Bool_vector x -> x
  | v -> raise_s [%message "Expected a bool vector" (v : t)]

let to_offset_exn = function
  | Offset x -> x
  | v -> raise_s [%message "Expected an offset" (v : t)]

let union x x' =
  bool_vector
  @@ Array.map2_exn (to_bool_vector_exn x) (to_bool_vector_exn x') ~f:( || )

let inter x x' =
  bool_vector
  @@ Array.map2_exn (to_bool_vector_exn x) (to_bool_vector_exn x') ~f:( && )

let sub x x' =
  bool_vector
  @@ Array.map2_exn (to_bool_vector_exn x) (to_bool_vector_exn x')
       ~f:(fun a b -> a && not b)

let sphere params (s : Op.sphere) =
  params.bench.input
  |> Array.map ~f:(fun v -> Float.(Vector3.l2_dist s.center v <= s.radius))
  |> bool_vector

let cylinder params (c : Op.cylinder) l h =
  let l = to_offset_exn l and h = to_offset_exn h in
  params.bench.input
  |> Array.map ~f:(fun v ->
         let open Vector3 in
         let rot = inverse_rotate v ~theta:c.theta in
         let in_radius =
           Float.(square (rot.y - c.y) + square (rot.z - c.z) < square c.radius)
         in
         let above_lo = Float.(rot.x >= Offset.offset l)
         and below_hi = Float.(rot.x <= Offset.offset h) in
         in_radius && above_lo && below_hi)
  |> bool_vector

let cuboid params (c : Op.cuboid) lx hx ly hy lz hz =
  let lx = to_offset_exn lx
  and hx = to_offset_exn hx
  and ly = to_offset_exn ly
  and hy = to_offset_exn hy
  and lz = to_offset_exn lz
  and hz = to_offset_exn hz in
  params.bench.input
  |> Array.map ~f:(fun v ->
         let open Vector3 in
         let rot = inverse_rotate v ~theta:c.theta in
         let above_lox = Float.(rot.x >= Offset.offset lx)
         and below_hix = Float.(rot.x <= Offset.offset hx)
         and above_loy = Float.(rot.y >= Offset.offset ly)
         and below_hiy = Float.(rot.y <= Offset.offset hy)
         and above_loz = Float.(rot.z >= Offset.offset lz)
         and below_hiz = Float.(rot.z <= Offset.offset hz) in
         above_lox && below_hix && above_loy && below_hiy && above_loz
         && below_hiz)
  |> bool_vector

let eval params op args =
  let open Util in
  match op with
  | Op.Union -> apply2 union args
  | Inter -> apply2 inter args
  | Sub -> apply2 sub args
  | Cylinder c -> apply2 (cylinder params c) args
  | Cuboid c -> apply6 (cuboid params c) args
  | Sphere s -> sphere params s
  | Offset x -> offset x
