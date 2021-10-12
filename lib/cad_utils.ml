let move_to x y s =
  let op' =
    match Cad_op.value s with
    | Cad_op.Circle c -> Cad_op.Circle { c with center = Vector2.{ x; y } }
    | Rect r ->
        let w = r.hi_right.x -. r.lo_left.x and h = r.hi_right.y -. r.lo_left.y in
        let lo_left = Vector2.{ x = x -. (w /. 2.0); y = y -. (h /. 2.0) }
        and hi_right = Vector2.{ x = x +. (w /. 2.0); y = y +. (h /. 2.0) } in
        Rect { r with lo_left; hi_right }
    | op -> raise_s [%message "cannot move" (op : Cad_op.op)]
  in
  Cad_op.create op'

let grid xmax ymax n shape =
  let ret =
    List.range 0 n
    |> List.map ~f:(fun x ->
           List.range 0 n
           |> List.map ~f:(fun y ->
                  let x = Float.(round @@ (of_int x * of_int xmax / of_int n))
                  and y = Float.(round @@ (of_int y * of_int ymax / of_int n)) in
                  move_to x y shape))
    |> List.concat
  in
  [%test_result: int] ~expect:(n * n) (List.length ret);
  ret
