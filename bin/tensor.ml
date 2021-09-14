open Core
open Staged_synth
open Std

let () =
  Random.init 0;

  let input = Tensor.Op.Id (Owl.Arr.of_array [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0 |] [| 1; 6 |]) in
  let p =
    let open Tensor.Op in
    let open Program in
    apply Permute ~args:[ apply Reshape ~args:[ apply input; vec_of_list [ 3; 2 ] ]; vec_of_list [ 1; 0 ] ]
  in

  let target = Program.eval (Tensor.Value.eval ()) p in
  print_s [%message "concrete target" (target : Tensor.Value.t)];

  let ops = Tensor.Op.[ Permute; Reshape; Flip; Cons; Int 1; Int 2; Int 3; Vec; input ] in
  print_s [%message (ops : Tensor.Op.t list)];

  Abstract_synth_tensor.synth target ops
