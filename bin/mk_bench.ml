open! Core
open! Core_bench
open Staged_synth

let main () =
  let module Code = Cstage.Code () in
  let module Deepcoder = Deepcoder.Make (Code) in
  let module Sketch = struct
    let inputs = [ "L" ]

    let output = "L"
  end in
  let module DeepSynth =
    Synth.Make (Sketch) (Code) (Deepcoder.Lang) (Deepcoder.Cache)
  in
  let g = DeepSynth.search_graph 10 in
  Out_channel.with_file "search.dot" ~f:(fun ch -> Synth.G.output_graph ch g);
  let synth = DeepSynth.enumerate 10 in
  print_endline (Code.to_string synth)

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate a synthesizer."
    [%map_open
      let () = Log.param in
      main]
  |> Command.run
