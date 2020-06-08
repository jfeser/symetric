open! Core
open Staged_synth.Lazy_cegis

let () =
  let conv l =
    List.map l ~f:(fun i -> if i = 0 then false else true) |> Array.of_list
  in
  let inputs =
    List.map ~f:conv [ [ 0; 1; 1; 0 ]; [ 1; 1; 0; 0 ]; [ 0; 0; 0; 1 ] ]
  in
  let output = conv [ 0; 1; 0; 1 ] in
  synth inputs output
