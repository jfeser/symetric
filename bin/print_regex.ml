open Core
open Symetric
open Std

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:{|
|}
    [%map_open
      let nl = flag "-nl" no_arg ~doc:" use natural language" in
      fun () ->
        let bench = In_channel.input_all In_channel.stdin in
        let examples = Regex_bench.load_examples_str bench in
        if nl then Fmt.pr "Prompt:\n%s\n\n" (Regex_bench.load_prompt bench);
        Fmt.pr "Positive:\n";
        List.iter examples ~f:(fun (s, b) -> if b then Fmt.pr " - \"%s\"\n" s);
        Fmt.pr "\nNegative:\n";
        List.iter examples ~f:(fun (s, b) -> if not b then Fmt.pr " - \"%s\"\n" s)]
  |> Command_unix.run
