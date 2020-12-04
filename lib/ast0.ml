open Base_quickcheck

module Op = struct
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

  type 'o t =
    | Union
    | Inter
    | Sub
    | Sphere of sphere
    | Cylinder of cylinder
    | Cuboid of cuboid
    | Offset of 'o
  [@@deriving compare, hash, sexp]
end
