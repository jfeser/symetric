open! Std
open Cad_ext

let size = Scene.Size.create ~xres:30 ~yres:30 ()

let jaccard (v : Value.t) (v' : Value.t) =
  match (v, v') with Scene s, Scene s' -> Scene.jaccard s s' | _ -> Float.infinity

let jaccard_edges size (v : Value.t) (v' : Value.t) =
  match (v, v') with Scene s, Scene s' -> Scene.(jaccard (edges size s) (edges size s')) | _ -> Float.infinity

let corner size (v : Value.t) (v' : Value.t) =
  match (v, v') with
  | Scene s, Scene s' ->
      let c = Scene.corners size s and c' = Scene.corners size s' in
      let c = List.map ~f:Scene.count c and c' = List.map ~f:Scene.count c' in
      List.map2_exn c c' ~f:(fun v v' -> (v - v') * (v - v'))
      |> List.sum (module Int) ~f:Fun.id
      |> Float.of_int |> Float.sqrt
  | _ -> Float.infinity

let gen_euclid v v' =
  match (v, v') with Value.Scene s, Value.Scene s' -> Scene.distance size s s' | _ -> Float.infinity

let distance v v' = (* jaccard_edges size v v' +. *) jaccard v v' +. corner size v v'

let distance = gen_euclid

let int x : Op.t Program.t = Apply (Int x, [])

let unnormalize = function
  | Program.Apply (Op.Int x, []) ->
      if x < -10 then [ Program.Apply (Op.Int (x + 1), []) ]
      else if x > 30 then [ Apply (Int (x - 1), []) ]
      else [ Program.Apply (Op.Int (x - 1), []); Apply (Int (x + 1), []) ]
  | Apply (Circle, [ Apply (Int x, []); Apply (Int y, []); Apply (Int r, []) ]) ->
      let lx = Int.round_down ~to_multiple_of:5 @@ (x - r)
      and ly = Int.round_down ~to_multiple_of:5 @@ (y - r)
      and hx = Int.round_up ~to_multiple_of:5 @@ (x + r)
      and hy = Int.round_up ~to_multiple_of:5 @@ (y + r) in
      [ Apply (Rect, [ int lx; int ly; int hx; int hy ]) ]
  | Apply (Op.Union, [ s; s' ]) -> [ s; s' ]
  | _ -> []

let normalize : Op.t Program.t -> Op.t Program.t = function
  | Apply (Circle, [ Apply (Int x, []); Apply (Int y, []); Apply (Int r, []) ]) as c ->
      let lx = Int.round_down ~to_multiple_of:5 @@ (x - r)
      and ly = Int.round_down ~to_multiple_of:5 @@ (y - r)
      and hx = Int.round_up ~to_multiple_of:5 @@ (x + r)
      and hy = Int.round_up ~to_multiple_of:5 @@ (y + r) in
      Apply (Union, [ c; Apply (Rect, [ int lx; int ly; int hx; int hy ]) ])
  | Apply (Rect, [ Apply (Int lx, []); Apply (Int ly, []); Apply (Int hx, []); Apply (Int hy, []) ]) ->
      Apply
        ( Rect,
          [
            Apply (Int (Int.round_down lx ~to_multiple_of:5), []);
            Apply (Int (Int.round_down ly ~to_multiple_of:5), []);
            Apply (Int (Int.round_up hx ~to_multiple_of:5), []);
            Apply (Int (Int.round_up hy ~to_multiple_of:5), []);
          ] )
  | Apply (Repl, [ p; dx; dy; _ ]) -> Apply (Repl, [ p; dx; dy; Apply (Int 5, []) ])
  | Apply (Int x, []) -> Apply (Int (Int.round_nearest x ~to_multiple_of:10), [])
  | p -> p

let run_repl () =
  let target = Op.(repl 0 8 3 @@ rect 7 5 13 9) in
  let start = Op.(repl 0 9 8 @@ rect 8 4 13 8) in

  let ectx = Value.Ctx.create size in
  let eval = Program.eval (Value.eval ectx) in
  let target_value = eval target in
  Local_search.of_unnormalize_tabu ~max_tabu:10 ~target:target_value ~dist:distance
    (module Op)
    (module Value)
    unnormalize eval start
  |> Iter.iter (fun p ->
         let value = eval p in
         if [%compare.equal: Value.t] value target_value then failwith "done";
         Fmt.pr "Program: %a@\nGoal: %a@\n%a@." (Program.pp Op.pp) p (Program.pp Op.pp) target (Value.pp ectx) value)

let run_e () =
  (* Union( *)
  (* Repl(Rect((Int 5), (Int 6), (Int 13), (Int 9)), (Int 0), (Int 16),  *)
  (*   (Int 12)), *)
  (* Repl(Rect((Int 5), (Int 5), (Int 11), (Int 17)), (Int 0), (Int 10),  *)
  (*   (Int 2))) *)
  let target = Op.(union (rect 5 5 7 25) (repl 0 8 3 @@ rect 7 5 13 9)) in
  let start = Op.(union (repl 0 16 12 (rect 5 6 13 9)) (repl 0 10 2 @@ rect 5 5 11 17)) in
  let ectx = Value.Ctx.create size in
  let eval = Program.eval (Value.eval ectx) in
  let target_value = eval target in
  Local_search.of_unnormalize_tabu ~max_tabu:100 ~target:target_value ~dist:distance
    (module Op)
    (module Value)
    unnormalize eval start
  |> Iter.iter (fun p ->
         let value = eval p in
         if [%compare.equal: Value.t] value target_value then failwith "done";
         Fmt.pr "Program: %a@\nGoal: %a@\n%a@." (Program.pp Op.pp) p (Program.pp Op.pp) target (Value.pp ectx) value)
