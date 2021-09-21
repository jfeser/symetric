open Core
open Staged_synth
open Std

let () =
  Random.init 0;

  let input = Tensor.Op.Id { elems = [ 1; 2; 3; 4; 5; 6 ]; shape = [ 1; 6 ] } in
  let p =
    let open Tensor.Op in
    let open Program in
    apply Permute ~args:[ apply Reshape ~args:[ apply input; vec_of_list [ 3; 2 ] ]; vec_of_list [ 2; 1 ] ]
  in

  let target = Program.eval (Tensor.Value.eval ()) p in
  print_s [%message "concrete target" (target : Tensor.Value.t)];

  let ops = Tensor.Op.[ Permute; Reshape; Cons; Int 1; Int 2; Int 3; Vec; input ] in
  print_s [%message (ops : Tensor.Op.t list)];

  let (), abs_time = Synth_utils.timed (fun () -> Abstract_synth_tensor.synth target ops) in
  let (), local_time = Synth_utils.timed (fun () -> Local_synth_tensor.synth target ops) in
  Fmt.pr "Local: %a, Abs: %a" Time.Span.pp local_time Time.Span.pp abs_time
