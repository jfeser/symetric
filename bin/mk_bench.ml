open! Core
open! Core_bench.Std
open Staged_synth

let () =
  let module Code = Cstage.Code () in
  let module Deepcoder = Deepcoder.Make (Code) in
  let module DeepSynth = Synth.Make (Code) (Deepcoder.Lang) (Deepcoder.Cache)
  in
  let synth = DeepSynth.enumerate 3 (Deepcoder.Value.I (Code.int 7)) in
  print_endline (Code.to_string synth)
