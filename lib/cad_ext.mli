module Type : sig
  type t = Int | Rep_count | Scene | Error [@@deriving compare, equal, hash, sexp]

  val default : t
  val output : t
  val pp : Format.formatter -> t -> unit
end

module Op : sig
  type t = Union | Inter | Circle | Rect | Repl | Sub | Int of int | Rep_count of int
  [@@deriving compare, equal, hash, sexp, yojson]

  val default : t
  val cost : t -> int
  val ret_type : t -> Type.t
  val args_type : t -> Type.t list
  val arity : t -> int
  val is_commutative : t -> bool
  val pp : Format.formatter -> t -> unit
  val default_operators : xres:int -> yres:int -> t list

  (** Constructors *)

  val int : int -> t Program.t
  val rc : int -> t Program.t
  val circle : int -> int -> int -> t Program.t
  val rect : int -> int -> int -> int -> t Program.t
  val union : t Program.t -> t Program.t -> t Program.t
  val inter : t Program.t -> t Program.t -> t Program.t
  val repl : int -> int -> int -> t Program.t -> t Program.t
end

module Value : sig
  type t = Int of int | Rep_count of int | Scene of Scene2d.t | Error
  [@@deriving compare, equal, hash, sexp]

  val default : t
  val is_scene : t -> bool
  val is_error : t -> bool
  val pp : Format.formatter -> t -> unit
  val eval : error_on_trivial:bool -> dim:Scene2d.Dim.t -> Op.t -> t list -> t
  val distance : t -> t -> float
end

val parse : Sexp.t -> Op.t Program.t
val serialize_op : Op.t -> Sexp.t
val serialize : Op.t Program.t -> Sexp.t
