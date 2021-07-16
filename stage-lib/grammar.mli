open! Core

module Bind : sig
  type t = string [@@deriving compare, hash, sexp]

  include Comparator.S with type t := t

  val of_string : string -> t
end

type nonterm = string [@@deriving compare, sexp]

type 'n term = Nonterm of 'n | App of string * 'n term list | As of 'n term * Bind.t

val with_holes : ?fresh:Utils.Fresh.t -> 'a term -> nonterm term * ('a * int * string) list

module Untyped_term : sig
  type t = nonterm term [@@deriving compare, sexp]

  val pp : t Fmt.t

  val to_string : t -> string

  val map : ?nonterm:(nonterm -> t) -> ?app:(string -> t list -> t) -> ?as_:(t -> Bind.t -> t) -> t -> t
end

module Term : sig
  type 'a t = private Untyped_term.t [@@deriving compare, hash, sexp]

  val nonterm : nonterm -> [> `Open ] t

  val app : nonterm -> ([< `Open | `Closed ] as 's) t list -> 's t

  val as_ : ([< `Open | `Closed ] as 's) t -> Bind.t -> 's t

  val size : _ t -> int

  val non_terminals : _ t -> string list

  val n_holes : _ t -> int

  val to_string : _ t -> string

  val with_holes : ?fresh:Utils.Fresh.t -> [ `Open | `Closed ] t -> [ `Closed ] t * (nonterm * int * string) list

  val bindings : 'a t -> (Bind.t * 'a t) list

  val load : Sexp.t -> [> `Closed ] t
end

module Rule : sig
  type 's t [@@deriving compare, hash, sexp]

  val pp : _ t Fmt.t

  val lhs : _ t -> nonterm

  val rhs : _ t -> [ `Open | `Closed ] Term.t

  val semantics : 's t -> 's list

  val create : nonterm -> [< `Open | `Closed ] Term.t -> 's list -> 's t

  val of_tuple : nonterm * [ `Open | `Closed ] Term.t -> 's t
end

type 's t = 's Rule.t list [@@deriving compare, sexp]

val of_list : (nonterm * [ `Open | `Closed ] Term.t) list -> 's t

val rhs : _ t -> nonterm -> [ `Open | `Closed ] Term.t list

val lhs : _ t -> nonterm list

val inline : nonterm -> 's t -> 's t

(* val productions : (nonterm * 'a) list -> nonterm -> 'a list *)

val weighted_random : ?state:Random.State.t -> (float * 'a) list -> 'a

val sample : ?state:Random.State.t -> ?factor:float -> nonterm -> _ t -> [ `Closed ] Term.t

val sample_seq : ?state:Random.State.t -> ?factor:float -> nonterm -> _ t -> [ `Closed ] Term.t Base.Sequence.t

val to_preorder : 'a term -> ('a * int) term

val find_binding : Bind.t -> 'a term -> 'a term option

val non_terminals : 'a term -> 'a list
