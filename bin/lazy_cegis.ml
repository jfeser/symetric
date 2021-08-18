open! Core
open Staged_synth

let () =
  Command.group ~summary:"Run lazy CEGIS."
    [
      ("cad-diverse", Sampling_diverse.cli);
      ("cad-diverse-nn", Sampling_diverse_nn.cli);
      ("cad-local", Local_search_diverse.cli);
      (* ("cad-learn", Learned_cost.cli); *)
      ("cad-baseline", Baseline.cli (module Cad));
      ("cad-dump", Dump_states.cli);
      ("cad-term-baseline", Baseline.cli (module Cad_term));
    ]
  |> Command.run
