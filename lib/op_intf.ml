module type S = sig
  type type_

  type t [@@deriving compare, equal, hash, sexp]

  include Comparator.S with type t := t

  val pp : t Fmt.t

  val to_string : t -> string

  val arity : t -> int

  val type_ : t -> type_ list * type_

  val args_type : t -> type_ list

  val ret_type : t -> type_
end
