open Core
open Symetric

let cmd =
  Command.group ~summary:"symetric"
    [
      ("enumerate-cad", Enumerate_cad.cmd);
      ("enumerate-regex", Enumerate_regex.cmd);
      ("enumerate-tower", Enumerate_towers.cmd);
      ("metric-cad", Metric_synth_cad.cmd);
      ("metric-regex", Metric_synth_regex.cmd);
      ("metric-tower", Metric_synth_tower.cmd);
      ("abs-cad", Abs_synth_cad.cmd);
      ("abs-regex", Abs_synth_regex.cmd);
    ]
  |> Command_unix.run
