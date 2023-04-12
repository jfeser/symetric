open Core
open Symetric
open Std

let main program_text =
  try
    let examples = Regex_bench.load_examples In_channel.stdin in
    let program, _ =
      Regex_bench.parse_program program_text |> Regex_sketch_ast.convert_sketch
    in
    let program = List.hd_exn program in
    let ctx = Regex.Value.Ctx.create examples 0 in
    let result = Regex.Value.eval ctx (Regex.Op.Sketch program) [] in
    let dist = Regex.Value.target_distance ctx result in
    if dist >. 0. then exit 1 else exit 0
  with _ -> exit 2

let () = main (Sys.get_argv ()).(1)
