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

let sphere input (s : Csg_op.sphere) =
  Array.map input ~f:(fun v -> Float.(Vector3.l2_dist s.center v <= s.radius))

let cylinder (c : Csg_op.cylinder) input l h =
  Array.map input ~f:(fun v ->
      let open Vector3 in
      let rot = inverse_rotate v ~theta:c.theta in
      let in_radius =
        Float.(square (rot.y - c.y) + square (rot.z - c.z) < square c.radius)
      in
      let above_lo = Float.(rot.x >= l) and below_hi = Float.(rot.x <= h) in
      in_radius && above_lo && below_hi)

let cuboid (c : Csg_op.cuboid) input lx hx ly hy lz hz =
  Array.map input ~f:(fun v ->
      let open Vector3 in
      let rot = inverse_rotate v ~theta:c.theta in
      let above_lox = Float.(rot.x >= lx)
      and below_hix = Float.(rot.x <= hx)
      and above_loy = Float.(rot.y >= ly)
      and below_hiy = Float.(rot.y <= hy)
      and above_loz = Float.(rot.z >= lz)
      and below_hiz = Float.(rot.z <= hz) in
      above_lox && below_hix && above_loy && below_hiy && above_loz && below_hiz)

let eval params op args =
  let open Apply in
  let input = params.bench.Csg_bench0.input in
  let eval_offsets = List.map ~f:(fun x -> to_offset_exn x |> Offset.offset) in
  match op with
  | Csg_op.Union -> apply2 union args
  | Inter -> apply2 inter args
  | Sub -> apply2 sub args
  | Cylinder c -> apply2 (cylinder c input) @@ eval_offsets args |> bool_vector
  | Cuboid c -> apply6 (cuboid c input) @@ eval_offsets args |> bool_vector
  | Sphere s -> sphere input s |> bool_vector
  | Offset x -> offset x