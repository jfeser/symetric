open! Core
open Staged_synth

let random_cli =
  let open Command in
  let open Let_syntax in
  basic ~summary:"Random benchmarks"
    [%map_open
      let params = Dumb_params.Spec.cli Cad_gen_pattern.spec
      and seed = flag "seed" (optional_with_default 0 int) ~doc:" random seed" in
      fun () ->
        Random.init seed;
        Cad_gen_pattern.mk_dataset params]

let bench_cli =
  let open Command in
  let open Let_syntax in
  basic ~summary:"Random benchmarks"
    [%map_open
      let params = Dumb_params.Spec.cli Cad_gen_pattern.spec
      and seed = flag "seed" (optional_with_default 1 int) ~doc:" random seed" in
      fun () ->
        Random.init seed;
        Cad_gen_pattern.mk_bench params]

let () = Command.run @@ Command.group [ ("data", random_cli); ("bench", bench_cli) ] ~summary:""
