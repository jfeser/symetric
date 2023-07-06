val synthesize :
  ?log:(Yojson.Safe.t -> unit) ->
  Baseline.Params.t ->
  Tower.Value.t ->
  Tower.Op.t Program.t option

val cmd : Command.t
