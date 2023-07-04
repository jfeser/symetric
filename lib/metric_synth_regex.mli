module Params : sig
  type t = { max_int : int; sketch : string } [@@deriving yojson]

  val default_max_int : int
  val create : ?max_int:int -> sketch:string -> unit -> t
  val param : t Command.Param.t
end

val synthesize :
  Metric_synth.Params.t ->
  Params.t ->
  Regex.Value.Ctx.t * Regex.Op.t list ->
  Regex.Op.t Program.t option
