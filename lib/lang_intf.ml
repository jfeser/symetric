module type S = sig
  type bench

  type lparams

  type params = (bench, lparams) Params.t

  type symb

  module Type : sig
    type t [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t
  end

  module Op : sig
    type circle = { id : int; center : Vector2.t; radius : float }
    [@@deriving compare, hash, sexp]

    type rect = { id : int; lo_left : Vector2.t; hi_right : Vector2.t }
    [@@deriving compare, hash, sexp]

    type t = Merge | Union | Inter | Circle of circle | Rect of rect
    [@@deriving compare, hash, sexp]

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

    val eval : params -> Op.t -> t list -> t
  end

  module Abs : sig
    type t [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t

    val graphviz_pp : params -> t Fmt.t

    val contains : t -> Conc.t -> bool

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

    val of_conc : params -> Conc.t -> t

    val vars : t -> Set.M(Smt.Var).t

    val refine : params -> Smt.Expr.t -> Smt.state -> Abs.t -> t -> Set.M(Abs).t
  end

  module Bench : sig
    type t = bench [@@deriving of_sexp]

    val ops : t -> Op.t list

    val output : t -> Conc.t
  end
end
