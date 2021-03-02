type t = bool Map.M(Vector2).t [@@deriving compare, hash, sexp]

let eval params op args =
  match (op, args) with
  | Cad_op.Inter, [ s; s' ] ->
      Map.merge s s' ~f:(fun ~key -> function
        | `Left x | `Right x -> Some x | `Both (x, x') -> Some (x && x'))
  | Union, [ s; s' ] ->
      Map.merge s s' ~f:(fun ~key -> function
        | `Left x | `Right x -> Some x | `Both (x, x') -> Some (x || x'))
  | Circle c, [] ->
      Cad_bench.points params.Params.bench.Cad_bench.input
      |> List.map ~f:(fun k ->
             let v = Float.(Vector2.(l2_dist c.center k) < c.radius) in
             (k, v))
      |> Map.of_alist_exn (module Vector2)
  | _ -> raise_s [%message "Unexpected eval" (op : Cad_op.t)]

let rec eval_program params (Program.Apply (op, args)) =
  eval params op (List.map args ~f:(eval_program params))
