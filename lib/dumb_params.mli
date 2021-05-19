module type Param_intf = sig
  type t [@@deriving sexp_of]

  val name : string

  val to_csv : (t -> string) option

  val init : (Univ_map.Packed.t Command.Param.t, unit -> t) Either.t

  val key : t Univ_map.Key.t
end

type 'a param = (module Param_intf with type t = 'a)

type spec

type t

val cli : spec list -> t Command.Param.t

type 'a mk =
  name:string ->
  doc:string ->
  ?init:[ `Cli of 'a option | `Default of unit -> 'a ] ->
  ?csv:bool ->
  ?aliases:string list ->
  unit ->
  'a param

val int : int mk

val bool : bool mk

val float : float mk

val span_ref : name:string -> ?csv:bool -> unit -> Time.Span.t ref param

val float_ref : name:string -> ?csv:bool -> unit -> float ref param

val bool_ref :
  name:string -> ?default:bool -> ?csv:bool -> unit -> bool ref param

val const_str : name:string -> ?csv:bool -> string -> string param

val csv : t -> string

val csv_header : t -> string

val get : t -> 'a param -> 'a

val to_spec : 'a param -> spec

type packed = P : 'a param * 'a -> packed

val of_alist_exn : packed list -> t
