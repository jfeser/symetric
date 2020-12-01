module Type = struct
  type t = Vector | Offset of Offset_type.t [@@deriving compare, hash, sexp]
end

module Op = struct
  include Ast0.Op

  let pp fmt op =
    let str =
      match op with
      | Union -> "or"
      | Inter -> "and"
      | Sub -> "sub"
      | Sphere _ -> "sphere"
      | Cylinder _ -> "cylinder"
      | Cuboid _ -> "cuboid"
      | Offset x -> sprintf "offset_%f" @@ Offset.offset x
    in
    Fmt.pf fmt "%s" str

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
