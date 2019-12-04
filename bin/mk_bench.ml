open! Core
open! Core_bench
open Staged_synth

let main () =
  let module Code = Cstage.Code () in
  let module Deepcoder = Deepcoder.Make (Code) in
  let module DeepSynth = Synth.Make (Code) (Deepcoder.Lang) (Deepcoder.Cache) in
  let inputs =
    let open Code in
    Map.of_alist_exn
      (module String)
      [
        ( "i",
          Deepcoder.Value.A
            ( Array.const (Array.mk_type (Array.mk_type Int))
            @@ [|
                 Array.const (Array.mk_type Code.Int)
                   [|
                     Code.int 1; Code.int 2; Code.int 4; Code.int 5; Code.int 7;
                   |];
               |] ) );
      ]
  in
  let output =
    let open Code in
    Deepcoder.Value.I (Array.const (Array.mk_type Int) [| Code.int 9 |])
  in
  let synth = DeepSynth.enumerate 4 inputs output in
  print_endline (Code.to_string synth)

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate a synthesizer."
    [%map_open
      let () = Log.param in
      main]
  |> Command.run
