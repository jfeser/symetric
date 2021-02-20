module type S = sig
  type bench

  type symb

  module Type : sig
    type t [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t
  end

  module Op : sig
    type t [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t

    val pp : t Fmt.t

    val to_string : t -> string

    val arity : t -> int

    val type_ : t -> Type.t list * Type.t

    val args_type : t -> Type.t list

    val ret_type : t -> Type.t
  end

  module Conc : sig
    type t

    val eval : bench Params.t -> Op.t -> t list -> t
  end

  module Abs : sig
    type t [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t

    val graphviz_pp : bench Params.t -> t Fmt.t

    val top : bench Params.t -> Type.t -> t

    val contains : t -> Conc.t -> bool

    val is_subset : t -> of_:t -> bool

    val eval : bench Params.t -> Op.t -> t list -> t

    val to_symb : bench Params.t -> t -> symb Smt.t

    val roots : t list -> t list
  end

  module Symb : sig
    type t = symb [@@deriving sexp_of]

    val create : ?prefix:(int -> string) -> bench Params.t -> Type.t -> t Smt.t

    val eval : bench Params.t -> Op.t -> t list -> t Smt.t

    val equals : t -> t -> Smt.Expr.t

    val of_conc : bench Params.t -> Conc.t -> t

    val vars : t -> Set.M(Smt.Var).t

    val refine :
      bench Params.t -> Smt.Expr.t -> Smt.state -> Abs.t -> t -> Set.M(Abs).t
  end

  module Bench : sig
    type t = bench [@@deriving of_sexp]

    val ops : t -> Op.t list

    val output : t -> Conc.t
  end
end
