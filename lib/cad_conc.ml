type t = (float -> float -> bool) * Cad_bench.grid

let eval params op args =
  let f =
    match (op, args) with
    | Cad_op.Inter, [ (s, _); (s', _) ] -> fun x y -> s x y && s' x y
    | Union, [ (s, _); (s', _) ] -> fun x y -> s x y || s' x y
    | Circle c, [] ->
        fun x y -> Float.(Vector2.(l2_dist c.center { x; y }) < c.radius)
    | _ -> raise_s [%message "Unexpected eval" (op : Cad_op.t)]
  in
  (f, params.Params.bench.Cad_bench.input)
