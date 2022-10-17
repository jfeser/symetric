open Core
open Symetric
open Std

let main () =
  let fn = (Sys.get_argv ()).(1) in
  let examples = In_channel.with_file fn ~f:Regex_bench.load_examples in
  let ground, _ =
    In_channel.with_file fn ~f:Regex_bench.load_ground_truth
    |> Regex_sketch_ast.convert_sketch
  in
  let ground = List.hd_exn ground in
  let ctx = Regex.Value.Ctx.create examples 0 in
  let result = Regex.Value.eval ctx (Regex.Op.Sketch ground) [] in
  let dist = Regex.Value.target_distance ctx result in
  if dist >. 0. then
    raise_s [%message (dist : float) (ground : Regex.Op.sketch) (result : Regex.Value.t)]

let () = main ()
