open! Core
open Staged_synth
open Cstage

let () = Log.(setup_log Debug)

let () =
  let module C = Code () in
  let arr =
    C.let_locus @@ fun () ->
    C.Array.init (C.Array.mk_type C.Int) (C.int 5) (fun i ->
        C.let_locus @@ fun () -> C.genlet C.(i + int 1))
  in
  C.to_string arr |> print_endline

let () =
  let module C = Code () in
  let arr =
    C.let_locus @@ fun () ->
    C.Array.init (C.Array.mk_type C.Int) (C.int 5) (fun _ ->
        C.let_locus @@ fun () -> C.genlet C.(int 1 + int 2))
  in
  C.to_string arr |> print_endline
