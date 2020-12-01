module Op = struct
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
