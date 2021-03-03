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

let circles_and_rects n =
  let circs =
    List.init n ~f:(fun i ->
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
        hi_right = { x = 2.0 *. Float.of_int n; y = 2.0 };
      }
  in
  let prog =
    let open Program in
    List.map circs ~f:(fun c -> Apply (c, []))
    |> List.reduce ~f:(fun c c' -> Apply (Union, [ c; c' ]))
  in
  let ops = [ Inter; Union; r1 ] @ circs in
  (Option.value_exn prog, ops)

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

let input = { xmax = 100; ymax = 100 }

let params =
  Params.create
    Cad_bench.{ ops = []; input; output = Map.empty (module Vector2) }

let dump (prog, ops) =
  let conc = Program.eval (Cad_conc.eval params) prog in

  let output =
    List.init input.xmax ~f:(fun x ->
        List.init input.xmax ~f:(fun y ->
            let x = Float.of_int x and y = Float.of_int y in
            if Map.find_exn conc { x; y } then 1 else 0))
    |> List.concat
  in

  let bench = Cad_bench.Serial.{ ops; input; output } in
  print_s @@ [%sexp_of: Cad_bench.Serial.t] bench

let () = dump @@ circles_and_rects 10
