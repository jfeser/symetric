module T : sig
  type 'o t = Apply of 'o * 'o t list
end

type 'o t = 'o T.t = Apply of 'o * 'o t list
[@@deriving compare, equal, hash, sexp, yojson]

val pp : (Format.formatter -> 'a -> unit) -> 'a t Fmt.t
val apply : ?args:'a t list -> 'a -> 'a t
val eval : ('a -> 'b list -> 'b) -> 'a t -> 'b
val size : 'a t -> int
val ops : 'a t -> 'a list
val iter : 'a t -> ('a * 'a t list) Iter.t
val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
val map : f:('a -> 'a t list -> 'b) -> 'a t -> 'b t
val subprograms : 'a t -> 'a t Iter.t
val commutative_closure : is_commutative:('a -> bool) -> 'a t -> 'a t Iter.t

module Make (Op : sig
  type t [@@deriving compare, hash, sexp]
end) : sig
  type nonrec t = Op.t t [@@deriving compare, hash, sexp]

  val eval_memoized : (Op.t -> 'a list -> 'a) -> t -> 'a
end
