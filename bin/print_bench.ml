open Core
open Staged_synth

let print_bench fn =
  let bench = Cad.Bench.load fn in
  Cad_conc.pprint Fmt.stdout bench.output

let cli =
  let open Command in
  let open Let_syntax in
  basic ~summary:"Print benchmark"
    [%map_open
      let fn = anon ("file" %: string) in
      fun () -> print_bench fn]

let () = Command.run cli
