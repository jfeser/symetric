val synthesize :
  ?log:(Yojson.Safe.t -> unit) ->
  Metric_synth.Params.t ->
  Tower.Value.t ->
  Tower.Op.t Program.t option

val cmd : Command.t
