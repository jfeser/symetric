open! Core
open Staged_synth

let main () =
  Synth.Log.set_level (Some Debug);
  let module Core = Cstage_core.Make () in
  let module Code = Cstage.Code (Core) in
  let module Code = struct
    module Core = Cstage_core.Make ()

    include Core
    include Cstage.Code (Core)
    module Array = Cstage_array.Array (Core)
    module Float = Cstage_float.Make (Core)
    module Tuple_3 = Cstage_tuple.Tuple_3.Make (Core)
    module Tuple_4 = Cstage_tuple.Tuple_4.Make (Core)
  end in
  let module Cad = Cad.Make (Code) in
  let module Sketch = struct
    let inputs = [ "S"; "V" ]

    let output = "E"
  end in
  let open Synth.Make (Sketch) (Code) (Cad.Lang) (Cad.Cache) in
  let g = search_graph 10 in
  Out_channel.with_file "search.dot" ~f:(fun ch -> G.output_graph ch g);
  let synth = enumerate 10 in
  print_endline (Code.to_string synth)

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate a CAD synthesizer."
    [%map_open
      let () = Log.param in
      main]
  |> Command.run
