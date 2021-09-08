module type S = sig
  type type_

  type t [@@deriving compare, equal, hash, sexp]

  include Comparator.S with type t := t

  val arity : t -> int

  val args_type : t -> type_ list

  val ret_type : t -> type_
end
