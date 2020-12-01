type t [@@deriving compare, hash, sexp]

type ctx

val of_bench : Bench0.offset Bench.t -> t Bench.t * ctx

val of_list : Offset_type.t -> float list -> ctx

val of_type : ctx -> Offset_type.t -> t Sequence.t

val of_type_count : ctx -> Offset_type.t -> int

val offset : t -> float

val prev : t -> t option

val next : t -> t option

val type_ : t -> Offset_type.t
