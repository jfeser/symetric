val synthesize :
  ?log:(Yojson.Safe.t -> unit) ->
  Baseline.Params.t ->
  Regex_bench.t ->
  Regex.Op.t Program.t option

val cmd : Command.t
