module Params : sig
  type t = { distance : [ `Jaccard | `Relative ] } [@@deriving yojson]

  val create : ?distance:[ `Jaccard | `Relative ] -> unit -> t
  val param : t Command.Param.t
end

val synthesize :
  ?log:(Yojson.Safe.t -> unit) ->
  Metric_synth.Params.t ->
  Cad_ext.Params.t ->
  Params.t ->
  Cad_ext.Value.t ->
  Cad_ext.Op.t Program.t option

val cmd : Command.t
