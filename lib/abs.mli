module Bool_vector : sig
  type t = bool Map.M(Int).t [@@deriving compare, hash, sexp]

  type concrete = bool array

  include Comparator.S with type t := t

  val top : t

  val pp : t Fmt.t

  val graphviz_pp : Params.t -> t Fmt.t

  val meet : t -> t -> t

  val is_subset_a : t -> of_:t -> bool

  val lift : concrete -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val sub : t -> t -> t

  val mem : t -> int -> bool

  val set : t -> int -> bool -> t

  val add : t -> int -> bool -> t option

  val add_exn : t -> int -> bool -> t

  val contains : t -> concrete -> bool

  val width : t -> int

  val of_list_exn : (int * bool) list -> t
end

module Offset : sig
  type t [@@deriving compare, hash, sexp]

  type concrete = Offset.t

  val contains : t -> concrete -> bool

  val graphviz_pp : t Fmt.t

  val top : t

  val lo : t -> float

  val hi : t -> float

  val lift : concrete -> t

  val create : lo:float -> hi:float -> t option

  val split_exn : t -> concrete -> t list

  val exclude_exn : t -> concrete -> t list
end

type t = Bool_vector of Bool_vector.t | Offset of Offset.t
[@@deriving compare, hash, sexp]

include Comparator.S with type t := t

val bool_vector : Bool_vector.t -> t

val offset : Offset.t -> t

val map :
  bool_vector:(Bool_vector.t -> 'a) -> offset:(Offset.t -> 'a) -> t -> 'a

val top : Ast.Type.t -> t

val graphviz_pp : Params.t -> t Fmt.t

val to_bool_vector_exn : t -> Bool_vector.t

val to_offset_exn : t -> Offset.t

val union : t -> t -> t

val inter : t -> t -> t

val sub : t -> t -> t

val contains : t -> Conc.t -> bool

val lift : Conc.t -> t

val cylinder : Params.t -> Ast.Op.cylinder -> t -> t -> t

val cuboid : Params.t -> Ast.Op.cuboid -> t -> t -> t -> t -> t -> t -> t

val eval : Params.t -> Offset.concrete Ast.Op.t -> t list -> t
