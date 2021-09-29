open Core
open Staged_synth
open Std

let () =
  Random.init 0;
  let cost = 16 and n_elems = 10 in

  let filter_bench p = Program.count p ~f:(function Tensor.Op.Permute | Reshape -> true | _ -> false) > 1 in

  let input = Tensor.Op.Id { elems = List.init n_elems ~f:Fun.id; shape = [ n_elems ] } in
  let ops =
    let ints = List.init 10 ~f:(fun i -> Tensor.Op.Int (i + 1)) in
    Tensor.Op.[ Permute; Reshape; Cons; Vec; input ] @ ints
  in

  let benchmarks =
    Generate_bench.generate_benchmarks ~max_states:300_000
      (module Tensor)
      ops (Tensor.Value.Ctx.of_params ()) cost Tensor.Type.output
    |> Iter.filter filter_bench |> Iter.take 25 |> Iter.persistent
  in
  print_s [%message (benchmarks : Tensor.Op.t Program.t Iter.t)];

  let results =
    benchmarks
    |> Iter.map (fun p ->
           let target = Program.eval (Tensor.Value.eval ()) p in
           print_s [%message "concrete target" (target : Tensor.Value.t)];

           print_s [%message (ops : Tensor.Op.t list)];

           let abs = Synth_utils.time (fun () -> Abstract_synth_tensor.synth cost target ops) in
           let local = Synth_utils.time (fun () -> Local_synth_tensor.synth cost target ops) in
           (* let local_no_rules = *)
           (*   Synth_utils.time (fun () -> Local_synth_tensor.synth ~use_rules:false cost target ops) *)
           (* in *)
           (abs, local))
    |> Iter.to_list
  in
  Fmt.pr "abs,local\n";
  List.iter results ~f:(fun (abs, local) -> Fmt.pr "%a,%a\n" Time.Span.pp abs Time.Span.pp local)
