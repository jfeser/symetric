module type Lang_intf = sig
  module Type : sig
    type t [@@deriving compare, equal, hash, sexp]

    val default : t
    val output : t
  end

  module Op : sig
    type t [@@deriving compare, equal, hash, sexp]

    val default : t
    val pp : t Fmt.t
    val cost : t -> int
    val arity : t -> int
    val args_type : t -> Type.t list
    val ret_type : t -> Type.t
    val is_commutative : t -> bool
  end

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp]

    val default : t
    val eval : Op.t -> t list -> t
    val is_error : t -> bool
    val pp : t Fmt.t
  end
end

module type Domain_pred_intf = sig
  type concrete
  type op
  type t [@@deriving compare, equal, hash, sexp]

  val pp : t Fmt.t
  val cost : [ `Concrete of concrete | `Pred of t ] -> int
  val lift : concrete -> t Iter.t

  val implies :
    [ `Concrete of concrete | `Preds of t list ] ->
    [ `Concrete of concrete | `Preds of t list ] ->
    bool

  val eval : t -> concrete -> bool

  val transfer :
    op ->
    [ `Concrete of concrete | `Preds of t list ] list ->
    bool * concrete option * t list
end

module Make
    (Lang : Lang_intf)
    (Domain_pred : Domain_pred_intf
                     with type concrete := Lang.Value.t
                      and type op := Lang.Op.t) : sig
  module Pred : sig
    type t = [ `Concrete of Lang.Value.t | `Pred of Domain_pred.t ]

    include Comparator.S with type t := t
  end

  module Abs_value : sig
    type t = Bottom | Preds of Core.Set.M(Pred).t
  end

  val synth :
    (Abs_value.t -> bool) ->
    (Lang.Value.t -> bool) ->
    Lang.Op.t list ->
    ((Lang.Op.t * Lang.Value.t * Abs_value.t) Program.t
    * Core.Set.M(Pred).t
    * Core.Set.M(Pred).t)
    Base.Queue.t

  val synth_simple :
    Lang.Value.t ->
    Lang.Op.t list ->
    ((Lang.Op.t * Lang.Value.t * Abs_value.t) Program.t
    * Core.Set.M(Pred).t
    * Core.Set.M(Pred).t)
    Base.Queue.t
end
