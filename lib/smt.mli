module Decl : sig
  type t
end

module Defn : sig
  type t
end

module Var = String_id

module Expr : sig
  type t [@@deriving sexp]

  val vars : t -> Set.M(Var).t

  val pp : t Fmt.t
end

module Model : sig
  type t = bool Map.M(Var).t [@@deriving sexp]

  val to_expr : t -> Expr.t

  val of_ : ?vars:Set.M(Var).t -> Expr.t -> t list
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

val make_decl : ?n_args:int -> string -> Var.t t

val fresh_decl : ?n_args:int -> ?prefix:string -> unit -> Var.t t

val make_defn : ?n_args:int -> string -> Expr.t -> Var.t t

val fresh_defn : ?n_args:int -> ?prefix:string -> Expr.t -> Var.t t

val fresh_defn_or_literal :
  ?n_args:int ->
  ?prefix:string ->
  literal:(bool -> 'a t) ->
  defn:(Var.t -> 'a t) ->
  Expr.t ->
  'a t

val annotate : string -> string -> Expr.t -> Expr.t

val comment : string -> Expr.t -> Expr.t

val var : Var.t -> Expr.t

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
    type t = int
  end

  val set_group : Group.t -> unit t

  val group_vars : (Group.t -> Set.M(Var).t) t
end

val read_input : ?parse_pos:Sexp.Parse_pos.t -> In_channel.t -> Sexp.t

val with_mathsat : ((unit -> Sexp.t) -> (string list -> unit) -> 'a) -> 'a

val error : Sexp.t -> 'a

val parse_model : Sexp.t -> (Var.t * bool) list

val get_interpolant_or_model_inner :
  Interpolant.Group.t list ->
  string list ->
  (unit -> Sexp.t) ->
  (string list -> unit) ->
  (Expr.t Or_error.t, (Var.t * bool) list) Either.t t

val get_interpolant_or_model :
  Interpolant.Group.t list ->
  (Expr.t Or_error.t, (Var.t * bool) list) Either.t t

val get_model : (Var.t * bool) list option t

val check_sat : bool t

val smtlib : string t

val with_comment_block : name:string -> ?descr:Sexp.t -> 'a t -> 'a t
