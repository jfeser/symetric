module Decl : sig
  type t
end

module Defn : sig
  type t
end

module Expr : sig
  type t [@@deriving sexp]

  type var = String_id.t [@@deriving compare, hash, sexp]

  val vars : t -> Set.M(String_id).t

  val pp : t Fmt.t
end

type stmt

type state

type 'a t

include Monad.S with type 'a t := 'a t

val with_state : state -> 'a t -> 'a * state

val run : 'a t -> 'a * state

val eval : 'a t -> 'a

val eval_with_state : state -> 'a t -> 'a

val clear_asserts : unit t

val make_decl : ?n_args:int -> string -> Expr.var t

val fresh_decl : ?n_args:int -> ?prefix:string -> unit -> Expr.var t

val make_defn : ?n_args:int -> string -> Expr.t -> Expr.var t

val fresh_defn : ?n_args:int -> ?prefix:string -> Expr.t -> Expr.var t

val annotate : string -> string -> Expr.t -> Expr.t

val comment : string -> Expr.t -> Expr.t

val var : Expr.var -> Expr.t

val var_s : string -> Expr.t

val bool : bool -> Expr.t

val true_ : Expr.t

val false_ : Expr.t

val exactly_one : Expr.t list -> Expr.t

val ( = ) : Expr.t -> Expr.t -> Expr.t

val ( && ) : Expr.t -> Expr.t -> Expr.t

val ( || ) : Expr.t -> Expr.t -> Expr.t

val and_ : Expr.t list -> Expr.t

val or_ : Expr.t list -> Expr.t

val ( => ) : Expr.t -> Expr.t -> Expr.t

val not : Expr.t -> Expr.t

val assert_ : Expr.t -> unit t

module Interpolant : sig
  module Group : sig
    type t

    type 'a s

    val create : t s
  end
  with type 'a s := 'a t

  val assert_group : ?group:Group.t -> Expr.t -> unit t
end

val read_input : ?parse_pos:Sexp.Parse_pos.t -> In_channel.t -> Sexp.t

val with_mathsat : ((unit -> Sexp.t) -> (string list -> unit) -> 'a) -> 'a

val error : Sexp.t -> 'a

val parse_model : Sexp.t -> (Expr.var * bool) list

val get_interpolant_or_model_inner :
  Interpolant.Group.t list ->
  string list ->
  (unit -> Sexp.t) ->
  (string list -> unit) ->
  (Expr.t Or_error.t, (Expr.var * bool) list) Either.t t

val get_interpolant_or_model :
  Interpolant.Group.t list ->
  (Expr.t Or_error.t, (Expr.var * bool) list) Either.t t

val get_model : (Expr.var * bool) list option t

val check_sat : bool t
