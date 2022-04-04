open Core
open Staged_synth
open Std
open Cad_ext
module S = Metric_synth_cad.S

let main name program =
  let ctx = Value.Ctx.create (Scene2d.Dim.create ~xres:16 ~yres:16 ~scaling:2 ()) in
  let target = Program.eval (Value.eval ctx) program in
  let mean_dist =
    Program.subprograms program |> Iter.drop 1
    |> Iter.filter_map (fun p ->
           match Program.eval (Value.eval ctx) p with
           | Scene _ as v ->
               let d = Value.distance v target in
               let size = Program.size p in
               Some (d, size)
           | _ -> None)
    |> Iter.filter (fun (_, size) -> size <= 5)
    |> Iter.map (fun (d, _) -> d)
    |> Iter.mean
  in
  Option.iter mean_dist ~f:(fun md -> Fmt.pr "%s %f\n" name md)

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:""
    [%map_open
      let bench_fn = anon ("BENCH" %: string) in
      fun () ->
        let bench = Sexp.load_sexp_conv_exn bench_fn parse in
        main bench_fn bench]
  |> Command_unix.run
