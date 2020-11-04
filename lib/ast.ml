module Type = struct
  type offset_kind = Cuboid_x | Cuboid_y | Cuboid_z | Cylinder
  [@@deriving compare, hash, sexp]

  type offset_type = { id : int; kind : offset_kind }
  [@@deriving compare, hash, sexp]

  type t = Vector | Offset of offset_type [@@deriving compare, hash, sexp]
end

module Op = struct
  type offset = { offset : float; type_ : Type.offset_type }
  [@@deriving compare, hash, sexp]

  type cylinder = {
    id : int;
    theta : Vector3.t;
    y : float;
    z : float;
    radius : float;
  }
  [@@deriving compare, hash, sexp]

  type cuboid = { id : int; theta : Vector3.t } [@@deriving compare, hash, sexp]

  type sphere = { center : Vector3.t; radius : float }
  [@@deriving compare, hash, sexp]

  type t =
    | Union
    | Inter
    | Sub
    | Sphere of sphere
    | Cylinder of cylinder
    | Cuboid of cuboid
    | Offset of offset
  [@@deriving compare, hash, sexp]

  let pp fmt op =
    let str =
      match op with
      | Union -> "or"
      | Inter -> "and"
      | Sub -> "sub"
      | Sphere _ -> "sphere"
      | Cylinder _ -> "cylinder"
      | Cuboid _ -> "cuboid"
      | Offset x -> sprintf "offset-%d" x.type_.id
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
    | Offset x -> Offset x.type_

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
