open Regex
open Value

let%expect_test "" =
  let ctx =
    Ctx.create
      [ ("1e", true); ("1", true); ("e", true); ("111e", true); ("eee11ee", true) ]
      0
  in
  print_s
    [%message
      (Program.eval (eval_unmemoized ctx) Op.(concat (P.apply num) (P.apply alpha)) : t)];
  print_s
    [%message
      (Program.eval (eval_unmemoized ctx)
         Op.(concat (P.apply num) @@ concat (P.apply num) (P.apply alpha))
        : t)];
  [%expect
    {|
      ( "Program.eval (eval_unmemoized ctx)\
       \n  (let open Op in concat (P.apply num) (P.apply alpha))"
       (Matches
        ((value (((0 (2))) () () ((2 (4))) ((4 (6))))) (holes ((buf "") (len 0))))))
      ( "Program.eval (eval_unmemoized ctx)\
       \n  (let open Op in\
       \n     (concat (P.apply num)) @@ (concat (P.apply num) (P.apply alpha)))"
       (Matches
        ((value (() () () ((1 (4))) ((3 (6))))) (holes ((buf "") (len 0)))))) |}]

let%expect_test "" =
  let ctx = Ctx.create [ ("1e", true); ("1", true); ("e", true); ("1.1e", true) ] 0 in
  print_s
    [%message (Program.eval (eval_unmemoized ctx) Op.(P.apply num || P.apply alpha) : t)];
  [%expect
    {|
      ( "Program.eval (eval_unmemoized ctx)\
       \n  (let open Op in (P.apply num) || (P.apply alpha))"
       (Matches
        ((value (((0 (1)) (1 (2))) ((0 (1))) ((0 (1))) ((0 (1)) (2 (3)) (3 (4)))))
         (holes ((buf "") (len 0)))))) |}]

let%expect_test "" =
  let ctx = Ctx.create [ ("1", true); ("11", true); ("111", true); ("1111", true) ] 0 in
  print_s
    [%message
      (Program.eval (eval_unmemoized ctx) Op.(repeat_range (P.apply num) 2 3) : t)];
  [%expect
    {|
      ( "Program.eval (eval_unmemoized ctx)\
       \n  (let open Op in repeat_range (P.apply num) 2 3)"
       (Matches
        ((value (() ((0 (2))) ((0 (2 3)) (1 (3))) ((0 (2 3)) (1 (3 4)) (2 (4)))))
         (holes ((buf "") (len 0)))))) |}]

let%expect_test "" =
  let ctx =
    Ctx.create
      [ ("white snake", true); ("white snake valley", true); ("white  snake", false) ]
      0
  in
  print_s [%message (Program.eval (eval_unmemoized ctx) Op.(empty) : t)];
  [%expect
    {|
      ("Program.eval (eval_unmemoized ctx) (let open Op in empty)"
       (Matches
        ((value
          (((0 (0)) (1 (1)) (2 (2)) (3 (3)) (4 (4)) (5 (5)) (6 (6)) (7 (7))
            (8 (8)) (9 (9)) (10 (10)) (11 (11)))
           ((0 (0)) (1 (1)) (2 (2)) (3 (3)) (4 (4)) (5 (5)) (6 (6)) (7 (7))
            (8 (8)) (9 (9)) (10 (10)) (11 (11)) (12 (12)) (13 (13)) (14 (14))
            (15 (15)) (16 (16)) (17 (17)) (18 (18)))
           ((0 (0)) (1 (1)) (2 (2)) (3 (3)) (4 (4)) (5 (5)) (6 (6)) (7 (7))
            (8 (8)) (9 (9)) (10 (10)) (11 (11)) (12 (12)))))
         (holes ((buf "") (len 0)))))) |}];
  print_s [%message (Program.eval (eval_unmemoized ctx) Op.(not empty) : t)];
  [%expect
    {|
      ("Program.eval (eval_unmemoized ctx) (let open Op in not empty)"
       (Matches
        ((value
          (((0 (1 2 3 4 5 6 7 8 9 10 11)) (1 (2 3 4 5 6 7 8 9 10 11))
            (2 (3 4 5 6 7 8 9 10 11)) (3 (4 5 6 7 8 9 10 11)) (4 (5 6 7 8 9 10 11))
            (5 (6 7 8 9 10 11)) (6 (7 8 9 10 11)) (7 (8 9 10 11)) (8 (9 10 11))
            (9 (10 11)) (10 (11)))
           ((0 (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18))
            (1 (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18))
            (2 (3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18))
            (3 (4 5 6 7 8 9 10 11 12 13 14 15 16 17 18))
            (4 (5 6 7 8 9 10 11 12 13 14 15 16 17 18))
            (5 (6 7 8 9 10 11 12 13 14 15 16 17 18))
            (6 (7 8 9 10 11 12 13 14 15 16 17 18))
            (7 (8 9 10 11 12 13 14 15 16 17 18)) (8 (9 10 11 12 13 14 15 16 17 18))
            (9 (10 11 12 13 14 15 16 17 18)) (10 (11 12 13 14 15 16 17 18))
            (11 (12 13 14 15 16 17 18)) (12 (13 14 15 16 17 18))
            (13 (14 15 16 17 18)) (14 (15 16 17 18)) (15 (16 17 18)) (16 (17 18))
            (17 (18)))
           ((0 (1 2 3 4 5 6 7 8 9 10 11 12)) (1 (2 3 4 5 6 7 8 9 10 11 12))
            (2 (3 4 5 6 7 8 9 10 11 12)) (3 (4 5 6 7 8 9 10 11 12))
            (4 (5 6 7 8 9 10 11 12)) (5 (6 7 8 9 10 11 12)) (6 (7 8 9 10 11 12))
            (7 (8 9 10 11 12)) (8 (9 10 11 12)) (9 (10 11 12)) (10 (11 12))
            (11 (12)))))
         (holes ((buf "") (len 0)))))) |}]

let%expect_test "" =
  let ctx = Ctx.create [ ("%%%", false) ] 0 in
  print_s
    [%message
      (Program.eval (eval_unmemoized ctx) Op.(repeat_at_least (P.apply @@ class_ '%') 4)
        : t)];
  [%expect
    {|
      ( "Program.eval (eval_unmemoized ctx)\
       \n  (let open Op in repeat_at_least (P.apply @@ (class_ '%')) 4)"
       (Matches
        ((value (((0 (0)) (1 (1)) (2 (2)) (3 (3))))) (holes ((buf "") (len 0)))))) |}];
  print_s
    [%message
      (Program.eval (eval_unmemoized ctx) Op.(repeat (P.apply @@ class_ '%') 4) : t)];
  print_s
    [%message
      (Program.eval (eval_unmemoized ctx)
         Op.(concat (P.apply @@ class_ '%') (P.apply @@ class_ '%'))
        : t)];
  print_s [%message (Program.eval (eval_unmemoized ctx) Op.(P.apply @@ class_ '%') : t)];
  [%expect {|
    ( "Program.eval (eval_unmemoized ctx)\
     \n  (let open Op in repeat (P.apply @@ (class_ '%')) 4)"
     (Matches ((value (())) (holes ((buf "") (len 0))))))
    ( "Program.eval (eval_unmemoized ctx)\
     \n  (let open Op in concat (P.apply @@ (class_ '%')) (P.apply @@ (class_ '%')))"
     (Matches ((value (((0 (2)) (1 (3))))) (holes ((buf "") (len 0))))))
    ("Program.eval (eval_unmemoized ctx) (let open Op in P.apply @@ (class_ '%'))"
     (Matches ((value (((0 (1)) (1 (2)) (2 (3))))) (holes ((buf "") (len 0)))))) |}]

let%expect_test "" =
  let input = "123456789.123" in
  let ctx = Ctx.create [ (input, true) ] 0 in
  print_s [%message (eval_unmemoized ctx (Op.class_ '.') [] : t)];
  [%expect
    {|
      ("eval_unmemoized ctx (Op.class_ '.') []"
       (Matches ((value (((9 (10))))) (holes ((buf "") (len 0)))))) |}];
  print_s [%message (eval_unmemoized ctx (Op.class_ '1') [] : t)];
  [%expect
    {|
        ("eval_unmemoized ctx (Op.class_ '1') []"
         (Matches ((value (((0 (1)) (10 (11))))) (holes ((buf "") (len 0)))))) |}];
  let concat =
    Program.(Apply (Op.Concat, [ Apply (Op.class_ '1', []); Apply (Op.class_ '2', []) ]))
  in
  print_s [%message (Program.eval (eval_unmemoized ctx) concat : t)];
  [%expect
    {|
        ("Program.eval (eval_unmemoized ctx) concat"
         (Matches ((value (((0 (2)) (10 (12))))) (holes ((buf "") (len 0)))))) |}];
  let repeat = Op.(repeat_range (P.apply Op.num) 1 3) in
  print_s [%message (Program.eval (eval_unmemoized ctx) repeat : t)];
  [%expect
    {|
      ("Program.eval (eval_unmemoized ctx) repeat"
       (Matches
        ((value
          (((0 (1 2 3)) (1 (2 3 4)) (2 (3 4 5)) (3 (4 5 6)) (4 (5 6 7)) (5 (6 7 8))
            (6 (7 8 9)) (7 (8 9)) (8 (9)) (10 (11 12 13)) (11 (12 13)) (12 (13)))))
         (holes ((buf "") (len 0)))))) |}];
  let ctx =
    Ctx.create
      [
        (* ("123456789.123", true); *)
        (* ("123456789123456.12", true); *)
        (* ("12345.1", true); *)
        ("123456789123456", true)
        (* ("1234567891234567", false); *)
        (* ("123.1234", false); *)
        (* ("1.12345", false); *)
        (* (".1234", false); *);
      ]
      0
  in
  let dot = P.apply (Op.class_ '.') in
  let empty = Op.empty in
  let opt = Op.optional dot in
  let repeat = Op.repeat_range (P.apply Op.num) 1 15 in
  let repeat_concat = Op.concat repeat opt in
  let repeat_val = (Program.eval (eval_unmemoized ctx) repeat : t) in
  let dot_val = (Program.eval (eval_unmemoized ctx) dot : t) in
  let empty_val = (Program.eval (eval_unmemoized ctx) empty : t) in
  let empty_or_dot_val = (Program.eval (eval_unmemoized ctx) Op.(empty || dot) : t) in
  let opt_val = (Program.eval (eval_unmemoized ctx) opt : t) in
  let repeat_concat_val = (Program.eval (eval_unmemoized ctx) repeat_concat : t) in
  print_s [%message (dot_val : t)];
  print_s [%message (empty_val : t)];
  print_s [%message (empty_or_dot_val : t)];
  print_s [%message (opt_val : t)];
  print_s [%message (repeat_val : t)];
  print_s [%message (repeat_concat_val : t)];
  print_s [%message (target_distance ctx repeat_concat_val : float)];
  [%expect
    {|
      (dot_val (Matches ((value (())) (holes ((buf "") (len 0))))))
      (empty_val
       (Matches
        ((value
          (((0 (0)) (1 (1)) (2 (2)) (3 (3)) (4 (4)) (5 (5)) (6 (6)) (7 (7))
            (8 (8)) (9 (9)) (10 (10)) (11 (11)) (12 (12)) (13 (13)) (14 (14))
            (15 (15)))))
         (holes ((buf "") (len 0))))))
      (empty_or_dot_val
       (Matches
        ((value
          (((0 (0)) (1 (1)) (2 (2)) (3 (3)) (4 (4)) (5 (5)) (6 (6)) (7 (7))
            (8 (8)) (9 (9)) (10 (10)) (11 (11)) (12 (12)) (13 (13)) (14 (14))
            (15 (15)))))
         (holes ((buf "") (len 0))))))
      (opt_val
       (Matches
        ((value
          (((0 (0)) (1 (1)) (2 (2)) (3 (3)) (4 (4)) (5 (5)) (6 (6)) (7 (7))
            (8 (8)) (9 (9)) (10 (10)) (11 (11)) (12 (12)) (13 (13)) (14 (14))
            (15 (15)))))
         (holes ((buf "") (len 0))))))
      (repeat_val
       (Matches
        ((value
          (((0 (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
            (1 (2 3 4 5 6 7 8 9 10 11 12 13 14 15))
            (2 (3 4 5 6 7 8 9 10 11 12 13 14 15))
            (3 (4 5 6 7 8 9 10 11 12 13 14 15)) (4 (5 6 7 8 9 10 11 12 13 14 15))
            (5 (6 7 8 9 10 11 12 13 14 15)) (6 (7 8 9 10 11 12 13 14 15))
            (7 (8 9 10 11 12 13 14 15)) (8 (9 10 11 12 13 14 15))
            (9 (10 11 12 13 14 15)) (10 (11 12 13 14 15)) (11 (12 13 14 15))
            (12 (13 14 15)) (13 (14 15)) (14 (15)))))
         (holes ((buf "") (len 0))))))
      (repeat_concat_val
       (Matches
        ((value
          (((0 (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
            (1 (2 3 4 5 6 7 8 9 10 11 12 13 14 15))
            (2 (3 4 5 6 7 8 9 10 11 12 13 14 15))
            (3 (4 5 6 7 8 9 10 11 12 13 14 15)) (4 (5 6 7 8 9 10 11 12 13 14 15))
            (5 (6 7 8 9 10 11 12 13 14 15)) (6 (7 8 9 10 11 12 13 14 15))
            (7 (8 9 10 11 12 13 14 15)) (8 (9 10 11 12 13 14 15))
            (9 (10 11 12 13 14 15)) (10 (11 12 13 14 15)) (11 (12 13 14 15))
            (12 (13 14 15)) (13 (14 15)) (14 (15)))))
         (holes ((buf "") (len 0))))))
      ("target_distance ctx repeat_concat_val" 0) |}]

let%expect_test "" =
  let input = "123456789.123" in
  let ctx = Ctx.create [ (input, true) ] 0 in
  let c1 = eval_unmemoized ctx (Op.class_ '1') [] in
  let c2 = eval_unmemoized ctx (Op.class_ '2') [] in
  let c3 =
    eval_unmemoized ctx
      (Op.Class (Multi { name = "12"; mem = (function '1' | '2' -> true | _ -> false) }))
      []
  in
  print_s [%message (c1 : t)];
  [%expect
    {| (c1 (Matches ((value (((0 (1)) (10 (11))))) (holes ((buf "") (len 0)))))) |}];
  print_s [%message (c2 : t)];
  [%expect
    {| (c2 (Matches ((value (((1 (2)) (11 (12))))) (holes ((buf "") (len 0)))))) |}];
  print_s [%message (c3 : t)];
  [%expect
    {|
      (c3
       (Matches
        ((value (((0 (1)) (1 (2)) (10 (11)) (11 (12)))))
         (holes ((buf "") (len 0)))))) |}];
  print_s [%message (distance c1 c2 : float) (distance c1 c3 : float)];
  [%expect {| (("distance c1 c2" 1) ("distance c1 c3" 0.5)) |}]
