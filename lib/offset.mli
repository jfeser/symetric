type t [@@deriving compare, hash, sexp]

type ctx

val of_bench : Bench.t -> ctx

val of_type : ctx -> Offset_type.t -> t Sequence.t

val offset : t -> float

val prev : t -> t option

val next : t -> t option

val type_ : t -> Offset_type.t
