open Core
open Symetric
open Std

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:{|
|}
    [%map_open
      let sketch = flag "-s" (optional string) ~doc:" sketch"
      and program_text = anon ("program" %: string) in
      fun () ->
        try
          let examples = Regex_bench.load_examples In_channel.stdin in
          let sketch = Option.map sketch ~f:Regex_bench.parse_program in
          let program = Regex_bench.parse_program program_text in
          let matches =
            Option.map sketch ~f:(fun sketch -> Regex_sketch_ast.matches program sketch)
            |> Option.value ~default:true
          in
          let program, _ = Regex_sketch_ast.convert_sketch program in
          let program = List.hd_exn program in
          let ctx = Regex.Value.Ctx.create examples 0 in
          let result = Regex.Value.eval ctx (Regex.Op.Sketch program) [] in
          let dist = Regex.Value.target_distance ctx result in
          if matches && dist =. 0. then exit 0
          else if dist =. 0. then exit 1
          else if matches then exit 2
          else exit 3
        with _ -> exit 4]
  |> Command_unix.run
