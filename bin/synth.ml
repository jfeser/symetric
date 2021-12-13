open! Core
open Staged_synth
module Core = Cstage_core.Make ()
module Code = Cstage.Code (Core)
module Deepcoder = Deepcoder.Make (Cstage_array.ArenaArray (Core)) (Code)

let main ~depth ~dump_graph ~sketch =
  let module Sketch = (val In_channel.with_file sketch ~f:Util.input_sketch : Sigs.SKETCH) in
  let open Synth.Make (Sketch) (Code) (Deepcoder.Lang) (Deepcoder.Cache) in
  Option.iter dump_graph ~f:(fun f ->
      let g = search_graph depth in
      Out_channel.with_file f ~f:(fun ch -> G.output_graph ch g));

  enumerate depth |> Code.to_string |> print_endline

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate a synthesizer."
    [%map_open
      let () = Log.param
      and dump_graph = flag "-dump-graph" (optional string) ~doc:" produce a search graph in DOT format"
      and depth = flag "-depth" (optional_with_default 10 int) ~doc:" the maximum size of program to evaluate"
      and sketch = anon ("SKETCH" %: string) in
      fun () -> main ~depth ~dump_graph ~sketch]
  |> Command.run
