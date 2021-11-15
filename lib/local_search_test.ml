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

let distance v v' = (* jaccard_edges size v v' +. *) jaccard v v' +. corner size v v'

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

let run () =
  let target = Op.(repl 0 8 3 @@ rect 7 5 13 9) in
  let start = Op.(repl 0 9 8 @@ rect 8 4 13 8) in

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
         Fmt.pr "Program: %a@\nGoal: %a@\n%a@." (Program.pp Op.pp) p (Program.pp Op.pp) target (Value.pp ectx)
           target_value)
