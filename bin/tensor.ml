open Core
open Staged_synth
open Std

let () =
  Random.init 0;
  let cost = 15 in

  let input = Tensor.Op.Id { elems = [ 1; 2; 3; 4; 5; 6 ]; shape = [ 1; 6 ] } in
  let ops =
    let ints = List.init 5 ~f:(fun i -> Tensor.Op.Int (i + 1)) in
    Tensor.Op.[ Permute; Reshape; Cons; Vec; input ] @ ints
  in

  let benchmarks =
    Generate_bench.generate_benchmarks (module Tensor) ops (Tensor.Value.Ctx.of_params ()) cost Tensor.Type.output
    |> Iter.take 5 |> Iter.persistent
  in
  print_s [%message (benchmarks : Tensor.Op.t Program.t Iter.t)];

  benchmarks
  |> Iter.iter (fun p ->
         let target = Program.eval (Tensor.Value.eval ()) p in
         print_s [%message "concrete target" (target : Tensor.Value.t)];

         print_s [%message (ops : Tensor.Op.t list)];

         let (), abs_time = Synth_utils.timed (fun () -> Abstract_synth_tensor.synth cost target ops) in
         let (), local_time = Synth_utils.timed (fun () -> Local_synth_tensor.synth cost target ops) in
         let (), local_time_no_rules =
           Synth_utils.timed (fun () -> Local_synth_tensor.synth ~use_rules:false cost target ops)
         in
         Fmt.pr "Local: %a, Local (no rules): %a, Abs: %a" Time.Span.pp local_time Time.Span.pp local_time_no_rules
           Time.Span.pp abs_time)
