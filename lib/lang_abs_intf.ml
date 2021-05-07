module type Comparable = sig
  type t [@@deriving compare, hash, sexp]

  include Comparator.S with type t := t
end

module type S = sig
  include Lang_intf.S

  module Abs : sig
    type t [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t

    val search_compare : params -> (module Comparable with type t = t)

    val graphviz_pp : params -> t Fmt.t

    val contains : t -> Value.t -> bool

    val leq : t -> t -> bool

    val eval : params -> Op.t -> t list -> t

    val to_symb : params -> t -> symb Smt.t

    val roots : t list -> t list
  end

  module Symb : sig
    type t = symb [@@deriving sexp_of]

    val create : ?prefix:(int -> string) -> params -> Type.t -> t Smt.t

    val eval : params -> Op.t -> t list -> t Smt.t

    val equals : t -> t -> Smt.Expr.t

    val of_conc : params -> Value.t -> t

    val vars : t -> Set.M(Smt.Var).t

    val refine : params -> Smt.Expr.t -> Smt.state -> Abs.t -> t -> Set.M(Abs).t
  end
end
