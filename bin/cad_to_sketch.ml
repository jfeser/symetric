open Core
open Staged_synth
open Cad_ext

let convert ~width ~height prog =
  let size = Scene2d.Dim.create ~xres:width ~yres:height () in
  let ectx = Value.Ctx.create size in
  let target_value = Program.eval (Value.eval ectx) prog in
  let target_scene = match target_value with Scene s -> s | _ -> assert false in
  let n = width * height in
  Fmt.pr "int NUM_DATA = %d;\n" n;
  Fmt.pr "Example[NUM_DATA] examples = {\n";
  let is_first = ref true in
  Scene2d.to_iter size target_scene (fun ((x, y), v) ->
      if !is_first then is_first := false else Fmt.pr ", ";
      Fmt.pr "new Example(x = %d, y = %d, v = %d)\n" x y (if v then 1 else 0));
  Fmt.pr "}\n";
  Fmt.pr {|#include "cad.sk"|};
  Fmt.pr "\n"

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Convert CAD problem to Sketch."
    [%map_open
      let width =
        flag "-scene-width" (optional_with_default 12 int) ~doc:" scene width in pixels"
      and height =
        flag "-scene-height" (optional_with_default 20 int) ~doc:" scene height in pixels"
      in
      fun () ->
        convert ~width ~height @@ Cad_ext.parse @@ Sexp.input_sexp In_channel.stdin]
  |> Command.run
