type t = { idx : int; arr : float array; [@compare.ignore] type_ : Csg_type.Offset.t }
[@@deriving compare, hash, sexp]

type ctx = float array Map.M(Csg_type.Offset).t [@@deriving sexp_of]

val of_list : Csg_type.Offset.t -> float list -> ctx
val of_type : ctx -> Csg_type.Offset.t -> t Sequence.t
val of_type_count : ctx -> Csg_type.Offset.t -> int
val offset : t -> float
val prev : t -> t option
val next : t -> t option
val type_ : t -> Csg_type.Offset.t
val lt : ctx -> Csg_type.Offset.t -> float -> t Sequence.t
val gt : ctx -> Csg_type.Offset.t -> float -> t Sequence.t
val idx : t -> int
