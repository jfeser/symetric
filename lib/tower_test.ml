open Tower

let%expect_test "" =
  let bridge =
    parse @@ Sexp.of_string "((for 3 (for 3 v (r 4) v (l 4)) (r 2) h (r 4)))"
  in
  let bridge_top = parse @@ Sexp.of_string "((r 2) (for 3 h (r 6)))" in
  let bridge_pillar = parse @@ Sexp.of_string "((for 3 (embed v (r 4) v)) (r 2) h)" in
  let bridge_pillar_part = parse @@ Sexp.of_string "((for 3 (embed v (r 4))))" in
  let ctx = Value.Ctx.create () in
  let target =
    match P.eval (Value.eval ctx) bridge with
    | Trans t -> (List.hd_exn t.summary).blocks
    | _ -> assert false
  in
  print_s
    [%message
      (Value.target_distance ctx target (P.eval (Value.eval ctx) bridge_top) : float)];
  print_s
    [%message
      (Value.distance ctx
         (P.eval (Value.eval ctx) bridge_pillar)
         (P.eval (Value.eval ctx) bridge_pillar_part)
        : float)];
  print_s
    [%message
      (Value.distance ctx
         (P.eval (Value.eval ctx) bridge_pillar)
         (P.eval (Value.eval ctx) bridge_top)
        : float)];
  print_s
    [%message
      (Value.target_distance ctx target (P.eval (Value.eval ctx) bridge_pillar) : float)];
  print_s
    [%message
      (Value.target_distance ctx target (P.eval (Value.eval ctx) bridge_pillar_part)
        : float)];
  [%expect {|
    ("Value.target_distance ctx target (P.eval (Value.eval ctx) bridge_top)" 36)
    ( "Value.distance ctx (P.eval (Value.eval ctx) bridge_pillar)\
     \n  (P.eval (Value.eval ctx) bridge_pillar_part)" 0.7612822906940554)
    ( "Value.distance ctx (P.eval (Value.eval ctx) bridge_pillar)\
     \n  (P.eval (Value.eval ctx) bridge_top)" 1)
    ("Value.target_distance ctx target (P.eval (Value.eval ctx) bridge_pillar)"
     14)
    ("Value.target_distance ctx target (P.eval (Value.eval ctx) bridge_pillar_part)"
     18) |}]
