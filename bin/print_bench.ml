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
      and scaling =
        flag "-scaling" (optional_with_default 1 int) ~doc:" scene scaling factor"
      in
      fun () ->
        let ectx =
          Value.Ctx.create
          @@ Scene2d.Dim.create ~xres:scene_width ~yres:scene_height ~scaling ()
        in
        let p = parse @@ Sexp.input_sexp In_channel.stdin in
        Fmt.pr "cost=%d\n" (Program.size p);
        Fmt.pr "%a\n" Value.pp @@ Program.eval (Value.eval ectx) p]
  |> Command.run
