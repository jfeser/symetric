module Bool_vector : sig
  type t [@@deriving compare, hash, sexp]

  type concrete = bool array

  include Comparator.S with type t := t

  val top : t

  val bot : t

  val graphviz_pp : Params.t -> t Fmt.t

  val meet : t -> t -> t

  val is_subset : t -> of_:t -> bool

  val lift : concrete -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val sub : t -> t -> t

  val add : t -> int -> bool -> t

  val contains : t -> concrete -> bool

  val width : t -> int

  val log_overlap : Params.t -> t -> t -> float
end

module Offset : sig
  type t [@@deriving compare, hash, sexp]

  type concrete = Offset.t

  val contains : t -> concrete -> bool

  val graphviz_pp : t Fmt.t

  val top : Offset.ctx -> Offset_type.t -> t

  val lo : t -> float

  val hi : t -> float

  val lift : concrete -> t

  val create : lo:float -> hi:float -> type_:Offset_type.t -> t option

  val split_exn : t -> concrete -> t list

  val exclude_exn : t -> concrete -> t list

  val type_ : t -> Offset_type.t
end

type t = Bool_vector of Bool_vector.t | Offset of Offset.t
[@@deriving compare, hash, sexp]

include Comparator.S with type t := t

val bool_vector : Bool_vector.t -> t

val offset : Offset.t -> t

val top : Params.t -> Ast.Type.t -> t

val meet : t -> t -> t

val is_bottom : t -> bool

val graphviz_pp : Params.t -> t Fmt.t

val to_bool_vector_exn : t -> Bool_vector.t

val to_offset_exn : t -> Offset.t

val union : t -> t -> t

val inter : t -> t -> t

val sub : t -> t -> t

val contains : t -> Conc.t -> bool

val lift : Conc.t -> t

val cylinder :
  Ast.Op.cylinder -> Vector3.t array -> Offset.t -> Offset.t -> Bool_vector.t

val cuboid :
  Ast.Op.cuboid ->
  Vector3.t array ->
  Offset.t ->
  Offset.t ->
  Offset.t ->
  Offset.t ->
  Offset.t ->
  Offset.t ->
  Bool_vector.t

val eval : Params.t -> Offset.concrete Ast.Op.t -> t list -> t

val is_subset : t -> of_:t -> bool

val to_symb : Params.t -> t -> Symb0.t Smt.t

val roots : t list -> t list