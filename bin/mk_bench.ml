open! Core
open Symetric

let main () =
  Synth.Log.set_level (Some Debug);
  let module Core = Cstage_core.Make () in
  let module Code = Cstage.Code (Core) in
  let module Deepcoder = Deepcoder.Make (Cstage_array.Array (Core)) (Code) in
  let module Sketch = struct
    let background = []
    let input = "L"
    let output = "L"
  end in
  let module DeepSynth = Synth.Make (Sketch) (Code) (Deepcoder.Lang) (Deepcoder.Cache) in
  let open DeepSynth in
  let g = search_graph 10 in
  Out_channel.with_file "search.dot" ~f:(fun ch -> G.output_graph ch g);
  let synth = DeepSynth.enumerate 10 in
  print_endline (Code.to_string synth)

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate a synthesizer."
    [%map_open
      let () = Log.param in
      main]
  |> Command.run
