open Core
open Symetric
open Cad_ext

let convert (size : Scene2d.Dim.t) prog =
  let ectx = Value.Ctx.create size in
  let target_value = Program.eval (Value.eval ectx) prog in
  let target_scene = match target_value with Scene s -> s | _ -> assert false in
  Scene2d.to_iter size target_scene (fun ((x, y), v) ->
      Fmt.pr "%d %d %d\n" x y (if v then 1 else 0))

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Convert CAD problem to pixels."
    [%map_open
      let dim = Scene2d.Dim.param in
      fun () -> convert dim @@ Cad_ext.parse @@ Sexp.input_sexp In_channel.stdin]
  |> Command_unix.run
