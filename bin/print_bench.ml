open Core
open Symetric
open Cad_ext

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Print a CAD benchmark."
    [%map_open
      let scene_width =
        flag "-scene-width" (optional_with_default 16 int) ~doc:" scene width in pixels"
      and scene_height =
        flag "-scene-height" (optional_with_default 16 int) ~doc:" scene height in pixels"
      and scaling =
        flag "-scaling" (optional_with_default 1 int) ~doc:" scene scaling factor"
      and numeric =
        flag "-numeric" no_arg ~doc:" print numeric output instead of ascii art"
      in
      fun () ->
        let dim = Scene2d.Dim.create ~xres:scene_width ~yres:scene_height ~scaling () in
        let ectx = Value.Ctx.create dim in
        let p = parse @@ Sexp.input_sexp In_channel.stdin in
        Cad_ext.error_on_trivial := false;
        let value = Program.eval (Value.eval ectx) p in
        if numeric then
          match value with
          | Scene s ->
              Scene2d.to_iter dim s
              |> Iter.iter (fun ((x, y), v) -> if v then Fmt.pr "%d %d@." x y)
          | _ -> assert false
        else Fmt.pr "%a@." Value.pp value]
  |> Command_unix.run
