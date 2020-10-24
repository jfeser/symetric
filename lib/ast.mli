module Type : sig
  type offset_kind = Cuboid_x | Cuboid_y | Cuboid_z | Cylinder
  [@@deriving compare, hash, sexp]

  type offset_type = { id : int; kind : offset_kind }
  [@@deriving compare, hash, sexp]

  type t = Vector | Offset of offset_type [@@deriving compare, hash, sexp]
end

module Op : sig
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

  val pp : t Fmt.t

  val arity : t -> int

  val type_ : t -> Type.t list * Type.t

  val ret_type : t -> Type.t

  val args_type : t -> Type.t list
end
