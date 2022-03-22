let move_to x y : Cad_ext.Op.t Program.t -> Cad_ext.Op.t Program.t =
  let int x : Cad_ext.Op.t Program.t = Apply (Int x, []) in
  function
  | Apply (Circle, [ _; _; radius ]) -> Apply (Circle, [ int x; int y; radius ])
  | Apply
      ( Rect,
        [
          Apply (Int lo_left_x, []);
          Apply (Int lo_left_y, []);
          Apply (Int hi_right_x, []);
          Apply (Int hi_right_y, []);
        ] ) ->
      let w = hi_right_x - lo_left_x and h = hi_right_y - lo_left_y in
      Apply
        ( Rect,
          [ int (x - (w / 2)); int (y - (h / 2)); int (x + (w / 2)); int (y + (h / 2)) ]
        )
  | p -> raise_s [%message "cannot move" (p : Cad_ext.Op.t Program.t)]

let grid xmax ymax n shape =
  let ret =
    List.range 0 n
    |> List.map ~f:(fun x ->
           List.range 0 n
           |> List.map ~f:(fun y ->
                  let x = Float.(to_int @@ (of_int x * of_int xmax / of_int n))
                  and y = Float.(to_int @@ (of_int y * of_int ymax / of_int n)) in
                  move_to x y shape))
    |> List.concat
  in
  [%test_result: int] ~expect:(n * n) (List.length ret);
  ret
