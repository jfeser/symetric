open Base_quickcheck

type cylinder = {
  id : int;
  theta : Vector3.t;
  y : float;
  z : float;
  radius : float;
}
[@@deriving compare, hash, quickcheck, sexp]

let quickcheck_generator_cylinder =
  let open Generator in
  let open Let_syntax in
  let%map theta = Vector3.quickcheck_generator
  and y = float_uniform_exclusive (-10.0) 10.0
  and z = float_uniform_exclusive (-10.0) 10.0
  and radius = float_uniform_exclusive (-10.0) 10.0 in
  { id = 0; theta; y; z; radius }

type cuboid = { id : int; theta : Vector3.t }
[@@deriving compare, hash, quickcheck, sexp]

let quickcheck_generator_cuboid =
  let open Generator in
  let open Let_syntax in
  let%map theta = Vector3.quickcheck_generator in
  { id = 0; theta }

type sphere = { center : Vector3.t; radius : float }
[@@deriving compare, hash, quickcheck, sexp]

module T = struct
  type t =
    | Union
    | Inter
    | Sub
    | Sphere of sphere
    | Cylinder of cylinder
    | Cuboid of cuboid
    | Offset of Offset.t
  [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

let pp fmt = function
  | Union -> Fmt.pf fmt "or"
  | Inter -> Fmt.pf fmt "and"
  | Sub -> Fmt.pf fmt "sub"
  | Sphere s ->
      Fmt.pf fmt "sphere_x%f_y%f_z%f_r%f" s.center.x s.center.y s.center.z
        s.radius
  | Cylinder c -> Fmt.pf fmt "cylinder_%d" c.id
  | Cuboid c -> Fmt.pf fmt "cuboid_%d" c.id
  | Offset x -> Fmt.pf fmt "offset_%f" @@ Offset.offset x

let to_string = Fmt.to_to_string pp

let arity = function
  | Sphere _ | Offset _ -> 0
  | Union | Inter | Sub | Cylinder _ -> 2
  | Cuboid _ -> 6

let ret_type = function
  | Union | Inter | Sub | Sphere _ | Cylinder _ | Cuboid _ -> Csg_type.Vector
  | Offset x -> Offset x.type_

let args_type = function
  | Union | Inter | Sub -> [ Csg_type.Vector; Vector ]
  | Sphere _ | Offset _ -> []
  | Cylinder c ->
      [
        Offset { id = c.id; kind = Cylinder };
        Offset { id = c.id; kind = Cylinder };
      ]
  | Cuboid c ->
      [
        Offset { id = c.id; kind = Cuboid_x };
        Offset { id = c.id; kind = Cuboid_x };
        Offset { id = c.id; kind = Cuboid_y };
        Offset { id = c.id; kind = Cuboid_y };
        Offset { id = c.id; kind = Cuboid_z };
        Offset { id = c.id; kind = Cuboid_z };
      ]

let type_ x = (args_type x, ret_type x)
