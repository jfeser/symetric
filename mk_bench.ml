open! Core
open! Core_bench.Std
open Deepcoder

let () =
  let module S = Cstage.T () in
  let module Deepcoder = Deepcoder.Make (S) in
  let module DeepSynth = Synth.Make (S) (Deepcoder.Lang) (Deepcoder.Cache) in
  let synth = S.enumerate 18 in
  [%code
    let synth () =
      try [%e synth (Deepcoder.Value.I [%code 7])]
      with Failure "Found solution" -> ()
    in
    let cmd =
      Core_bench.Std.Bench.make_command
        [ Core_bench.Std.Bench.Test.create ~name:"enum_staged" synth ]
    in
    Core.Command.run cmd]
  |> print Format.std_formatter
