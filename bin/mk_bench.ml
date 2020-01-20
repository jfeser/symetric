open! Core
open! Core_bench
open Staged_synth

let main () =
  let module Code = Cstage.Code () in
  let module Deepcoder = Deepcoder.Make (Code) in
  let module Examples = struct
    open Code
    open Deepcoder

    let inputs =
      [
        ( "L",
          Value.A
            ( Array.const (Array.mk_type (Array.mk_type int_t))
            @@ [|
                 Array.const (Array.mk_type int_t)
                   Int.[| int 3; int 7; int 5; int 2; int 8 |];
               |] ) );
      ]

    let output =
      ( "L",
        Value.A
          ( Array.const (Array.mk_type (Array.mk_type int_t))
          @@ [|
               Array.const (Array.mk_type int_t)
                 Int.[| int 3; int 2; int 5; int 2; int 3 |];
             |] ) )
  end in
  let module DeepSynth =
    Synth.Make (Code) (Deepcoder.Lang (Examples)) (Deepcoder.Cache)
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
