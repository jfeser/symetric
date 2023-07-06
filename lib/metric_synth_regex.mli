module Params : sig
  type t = { max_int : int } [@@deriving yojson]

  val default_max_int : int
  val create : ?max_int:int -> unit -> t
  val param : t Command.Param.t
end

val synthesize :
  ?log:(Yojson.Safe.t -> unit) ->
  Metric_synth.Params.t ->
  Params.t ->
  Regex.Value.Ctx.t * Regex.Op.t list ->
  Regex.Op.t Program.t option

val cmd : Command.t
