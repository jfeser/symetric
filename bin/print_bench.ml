open Core
open Staged_synth
open Cad_ext

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Print a CAD benchmark."
    [%map_open
      let scene_width =
        flag "-scene-width" (optional_with_default 12 int) ~doc:" scene width in pixels"
      and scene_height =
        flag "-scene-height" (optional_with_default 20 int) ~doc:" scene height in pixels"
      in
      fun () ->
        let ectx =
          Value.Ctx.create @@ Scene2d.Dim.create ~xres:scene_width ~yres:scene_height ()
        in
        Fmt.pr "%a\n" Value.pp
        @@ Program.eval (Value.eval ectx)
        @@ parse
        @@ Sexp.input_sexp In_channel.stdin]
  |> Command.run
