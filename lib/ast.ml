module Type = struct
  type t = Vector | Offset of Offset_type.t [@@deriving compare, hash, sexp]
end

module Op = struct
  include Ast0.Op

  let pp fmt = function
    | Union -> Fmt.pf fmt "or"
    | Inter -> Fmt.pf fmt "and"
    | Sub -> Fmt.pf fmt "sub"
    | Sphere s ->
        Fmt.pf fmt "sphere_x%f_y%f_z%f_r%f" s.center.x s.center.y s.center.z
          s.radius
    | Cylinder _ -> Fmt.pf fmt "cylinder"
    | Cuboid _ -> Fmt.pf fmt "cuboid"
    | Offset x -> Fmt.pf fmt "offset_%f" @@ Offset.offset x

  let arity = function
    | Sphere _ | Offset _ -> 0
    | Union | Inter | Sub | Cylinder _ -> 2
    | Cuboid _ -> 6

  let ret_type =
    let open Type in
    function
    | Union | Inter | Sub | Sphere _ | Cylinder _ | Cuboid _ -> Vector
    | Offset x -> Offset (Offset.type_ x)

  let args_type =
    let open Type in
    function
    | Union | Inter | Sub -> [ Vector; Vector ]
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
end
