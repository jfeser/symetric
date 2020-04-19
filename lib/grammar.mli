open! Core

type nonterm = string [@@deriving compare, sexp]

module Untyped_term : sig
  type t = Nonterm of nonterm | App of string * t list
  [@@deriving compare, sexp]

  val to_string : t -> string

  val map : ?nonterm:(nonterm -> t) -> ?app:(string -> t list -> t) -> t -> t
end

module Term : sig
  type 'a t = private Untyped_term.t [@@deriving compare, hash, sexp]

  val nonterm : nonterm -> [> `Open ] t

  val app : nonterm -> ([< `Open | `Closed ] as 's) t list -> 's t

  val size : _ t -> int

  val non_terminals : _ t -> string list

  val n_holes : _ t -> int

  val to_string : _ t -> string

  val with_holes :
    ?fresh:Utils.Fresh.t ->
    equal:(string -> string -> bool) ->
    _ t ->
    [ `Closed ] t * (nonterm * string) list
end

module Rule : sig
  type 's t [@@deriving compare, hash, sexp]

  val lhs : _ t -> nonterm

  val rhs : _ t -> [ `Open | `Closed ] Term.t

  val semantics : 's t -> 's list

  val of_tuple : nonterm * [ `Open | `Closed ] Term.t -> 's t
end

type 's t = 's Rule.t list [@@deriving compare, sexp]

val of_list : (nonterm * [ `Open | `Closed ] Term.t) list -> 's t

val rhs : _ t -> nonterm -> [ `Open | `Closed ] Term.t list

val non_terminals : _ t -> nonterm list

val inline : nonterm -> 's t -> 's t

val with_holes :
  ?fresh:Utils.Fresh.t ->
  [ `Open | `Closed ] Term.t ->
  [ `Closed ] Term.t * (nonterm * string) list

(* val productions : (nonterm * 'a) list -> nonterm -> 'a list *)

val weighted_random : ?state:Random.State.t -> (float * 'a) list -> 'a

val sample :
  ?state:Random.State.t -> ?factor:float -> nonterm -> _ t -> [ `Closed ] Term.t

val sample_seq :
  ?state:Random.State.t ->
  ?factor:float ->
  nonterm ->
  _ t ->
  [ `Closed ] Term.t Base.Sequence.t
