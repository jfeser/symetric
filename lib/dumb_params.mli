module Param : sig
  module type S = sig
    type t [@@deriving sexp_of]

    val name : string
    val to_json : (t -> Yojson.Basic.t) option
    val init : (Univ_map.Packed.t Command.Param.t, unit -> t) Either.t
    val key : t Univ_map.Key.t
  end

  type ('a, 'b) t
  type free
  type bound

  type 'a mk =
    name:string ->
    doc:string ->
    ?init:[ `Cli of 'a option | `Default of unit -> 'a ] ->
    ?json:bool ->
    ?aliases:string list ->
    unit ->
    ('a, free) t

  val create : (module S with type t = 'a) -> ('a, free) t
  val int : int mk
  val bool : bool mk
  val float : float mk
  val string : string mk
  val mut : ('a, free) t -> ('a ref, free) t
  val span_ref : name:string -> ?json:bool -> unit -> (Time.Span.t ref, free) t
  val float_ref : name:string -> ?json:bool -> unit -> (float ref, free) t
  val float_seq : name:string -> ?json:bool -> unit -> (float Queue.t, free) t
  val float_list : name:string -> ?json:bool -> unit -> (float list Queue.t, free) t
  val bool_ref : name:string -> ?default:bool -> ?json:bool -> unit -> (bool ref, free) t
  val const_str : name:string -> ?json:bool -> string -> (string, free) t

  val ids :
    (module Comparator.S with type t = 't) -> name:string -> doc:string -> (string * 't) list -> ('t list, free) t

  val symbol : name:string -> doc:string -> ?default:'a -> ?json:bool -> ('a * string) list -> ('a, free) t
end

type t

val get : t -> ('a, Param.bound) Param.t -> 'a
val set : t -> ('a, Param.bound) Param.t -> 'a -> t
val json : t -> Yojson.Basic.t

module Spec : sig
  type t
  type values

  val create : ?name:string -> unit -> t
  val inherit_ : t -> string -> t
  val add : t -> ('a, Param.free) Param.t -> ('a, Param.bound) Param.t
  val union : t list -> t
  val cli : t -> values Command.Param.t
end
with type values := t

type pair = P : ('a, Param.bound) Param.t * 'a -> pair

val of_alist_exn : pair list -> t
