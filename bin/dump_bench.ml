open! Core
open Staged_synth
open Cad_op
open Cad_bench

let circle_inter =
  let c1 = Circle { id = 0; center = { x = 2.0; y = 2.0 }; radius = 2.0 } in
  let c2 = Circle { id = 1; center = { x = 3.0; y = 2.0 }; radius = 2.0 } in
  let prog = Program.Apply (Inter, [ Apply (c1, []); Apply (c2, []) ]) in
  let ops = [ Inter; Union; c1; c2 ] in
  (prog, ops)

let circle_and_rect =
  let c1 = Circle { id = 0; center = { x = 2.0; y = 2.0 }; radius = 2.0 } in
  let r1 =
    Rect
      {
        id = 0;
        lo_left = { x = 0.0; y = 0.0 };
        hi_right = { x = 4.0; y = 4.0 };
      }
  in
  let prog = Program.Apply (c1, []) in
  let ops = [ Inter; Union; c1; r1 ] in
  (prog, ops)

let circles_and_rects =
  let ([ c1; c2; c3 ] as circs) =
    List.init 3 ~f:(fun i ->
        Circle
          {
            id = i;
            center = { x = Float.of_int @@ ((i * 2) + 1); y = 1.0 };
            radius = 1.0;
          })
  in
  let r1 =
    Rect
      {
        id = 0;
        lo_left = { x = 0.0; y = 0.0 };
        hi_right = { x = 6.0; y = 2.0 };
      }
  in
  let prog =
    Program.(
      Apply
        ( Union,
          [ Apply (c1, []); Apply (Union, [ Apply (c2, []); Apply (c3, []) ]) ]
        ))
  in
  let ops = [ Inter; Union; r1 ] @ circs in
  (prog, ops)

let circles_and_rects_unsat =
  let ([ c1; c2; c3 ] as circs) =
    List.init 3 ~f:(fun i ->
        Circle
          {
            id = i;
            center = { x = Float.of_int @@ ((i * 2) + 1); y = 1.0 };
            radius = 1.0;
          })
  in
  let r1 =
    Rect
      {
        id = 0;
        lo_left = { x = 0.0; y = 0.0 };
        hi_right = { x = 6.0; y = 2.0 };
      }
  in
  let prog = Program.(Apply (r1, [])) in

  let ops = [ Inter; Union ] @ circs in
  (prog, ops)

let xmax = 10

let ymax = 10

let input = { xmax = 10; ymax = 10 }

let params = Params.create Cad_bench.{ ops = []; input; output = [] }

let dump (prog, ops) =
  let conc = Program.eval (Cad_conc.eval params) prog in

  let output =
    List.init xmax ~f:(fun x ->
        List.init ymax ~f:(fun y ->
            let x = Float.of_int x and y = Float.of_int y in
            if Map.find_exn conc { x; y } then 1 else 0))
    |> List.concat
  in

  let bench = { ops; input; output } in
  print_s @@ [%sexp_of: Cad_bench.t] bench

let () = dump circles_and_rects_unsat
