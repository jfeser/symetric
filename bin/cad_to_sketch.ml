open Core
open Staged_synth
open Cad_ext

let convert (size : Scene2d.Dim.t) height prog =
  let ectx = Value.Ctx.create size in
  let target_value = Program.eval (Value.eval ectx) prog in
  let target_scene = match target_value with Scene s -> s | _ -> assert false in
  Fmt.pr "#include \"cad_header.sk\"\n";
  Fmt.pr "#define SCALING %d\n" @@ size.scaling;
  Fmt.pr "int NUM_DATA = %d;\n" @@ Scene2d.Dim.npixels size;
  Fmt.pr "Example[NUM_DATA] examples = {\n";
  let is_first = ref true in
  Scene2d.to_iter size target_scene (fun ((x, y), v) ->
      if !is_first then is_first := false else Fmt.pr ", ";
      Fmt.pr "new Example(x = %d, y = %d, v = %d)\n" x y (if v then 1 else 0));
  Fmt.pr "};\n";
  Fmt.pr "#define DEPTH %d\n" height;
  Fmt.pr {|#include "cad.sk"|};
  Fmt.pr "\n"

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Convert CAD problem to Sketch."
    [%map_open
      let dim = Scene2d.Dim.param
      and height =
        flag "-height" (required int) ~doc:" max height of the generated program"
      in
      fun () -> convert dim height @@ Cad_ext.parse @@ Sexp.input_sexp In_channel.stdin]
  |> Command_unix.run
