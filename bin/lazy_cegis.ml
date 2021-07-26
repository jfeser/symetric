open! Core
open Staged_synth

let () =
  Command.group ~summary:"Run lazy CEGIS."
    [
      ("cad-diverse", Sampling_diverse.cli (module Cad));
      ("cad-baseline", Baseline.cli (module Cad));
      ("cad-dump", Dump_states.cli (module Cad));
      ("cad-term-baseline", Baseline.cli (module Cad_term));
    ]
  |> Command.run
