open Core
open Symetric

let convert (dim : Scene2d.Dim.t) prog =
  let target_value =
    Program.eval (Cad_ext.Value.eval ~error_on_trivial:false ~dim) prog
  in
  let target_scene = match target_value with Scene s -> s | _ -> assert false in
  Scene2d.to_iter target_scene (fun (x, y, v) ->
      Fmt.pr "%d %d %d\n" x y (if v then 1 else 0))

let pixels =
  let open Command.Let_syntax in
  Command.basic ~summary:"Convert CAD problem to pixels."
    [%map_open
      let dim = Scene2d.Dim.param in
      fun () -> convert dim @@ Cad_ext.parse @@ Sexp.input_sexp In_channel.stdin]

let cmd =
  Command.group ~summary:"symetric"
    [
      ("enumerate-cad", Enumerate_cad.cmd);
      ("enumerate-regex", Enumerate_regex.cmd);
      ("enumerate-tower", Enumerate_towers.cmd);
      ("metric-cad", Metric_synth_cad.cmd);
      ("metric-regex", Metric_synth_regex.cmd);
      ("metric-tower", Metric_synth_tower.cmd);
      ("abs-cad", Abs_synth_cad.cmd);
      ("abs-regex", Abs_synth_regex.cmd);
      ("pixels", pixels);
    ]
  |> Command_unix.run
