module Params : sig
  type t = { dim : Scene2d.Dim.t; distance : [ `Jaccard | `Relative ] }
  [@@deriving yojson]

  val create : ?dim:Scene2d.Dim.t -> ?distance:[ `Jaccard | `Relative ] -> unit -> t
  val param : t Command.Param.t
end

val synthesize :
  Metric_synth.Params.t -> Params.t -> Cad_ext.Value.t -> Cad_ext.Op.t Program.t option
