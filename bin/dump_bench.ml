open! Core
open Staged_synth
open Cad_op
open Cad_bench

let c1 = Circle { id = 0; center = { x = 2.0; y = 2.0 }; radius = 2.0 }

let c2 = Circle { id = 1; center = { x = 3.0; y = 2.0 }; radius = 2.0 }

let p = Program.Apply (Inter, [ Apply (c1, []); Apply (c2, []) ])

let xmax = 10

let ymax = 10

let input = { xmax = 10; ymax = 10 }

let params =
  Params.create
    Cad_bench.
      {
        ops = [ Inter; Union; c1; c2 ];
        input = { xmax = 0; ymax = 0 };
        output = [];
      }

let conc = Program.eval (Cad_conc.eval params) p

let output =
  List.init xmax ~f:(fun x ->
      List.init ymax ~f:(fun y ->
          let x = Float.of_int x and y = Float.of_int y in
          if conc x y then 1 else 0))
  |> List.concat

let b = { ops = [ Inter; Union; c1; c2 ]; input; output }

let () = print_s @@ [%sexp_of: Cad_bench.t] b
