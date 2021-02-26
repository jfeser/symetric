type t = Set.M(Vector2).t [@@deriving compare, hash, sexp]

let eval params op args =
  match (op, args) with
  | Cad_op.Inter, [ s; s' ] -> Set.inter s s'
  | Union, [ s; s' ] -> Set.union s s'
  | Circle c, [] ->
      Cad_bench.points params.Params.bench.Cad_bench.input
      |> List.filter ~f:(fun v ->
             Float.(Vector2.(l2_dist c.center v) < c.radius))
      |> Set.of_list (module Vector2)
  | _ -> raise_s [%message "Unexpected eval" (op : Cad_op.t)]

let rec eval_program params (Program.Apply (op, args)) =
  eval params op (List.map args ~f:(eval_program params))
