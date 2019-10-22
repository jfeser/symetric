open! Core
open! Core_bench.Std
open Staged_synth

let main () =
  let module Code = Cstage.Code () in
  let module Deepcoder = Deepcoder.Make (Code) in
  let module DeepSynth = Synth.Make (Code) (Deepcoder.Lang) (Deepcoder.Cache)
  in
  let synth = DeepSynth.enumerate 4 (Deepcoder.Value.I (Code.int 7)) in
  print_endline (Code.to_string synth)

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate a synthesizer."
    [%map_open
      let () = Log.param in
      main]
  |> Command.run
