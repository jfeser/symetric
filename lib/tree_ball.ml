let rewrites ops t d f =
  let n = Program.size t in
  let rewrite = Array.create ~len:n None in
  Combinat.combinations (List.init n ~f:Fun.id) ~k:d @@ fun loc ->
  Combinat.sequences ops ~k:d @@ fun ops ->
  Array.fill rewrite ~pos:0 ~len:n None;
  Array.iteri loc ~f:(fun i l -> rewrite.(l) <- Some ops.(i));
  f rewrite

let ball ops t d f =
  rewrites ops t d @@ fun rewrite ->
  f @@ Program.mapi t ~f:(fun i op -> Option.value rewrite.(i) ~default:op)

let%expect_test "" =
  ball [ "x"; "y"; "z" ]
    Program.(apply "a" ~args:[ apply "b"; apply "c" ~args:[ apply "d" ] ])
    1
  @@ fun p -> print_s [%message (p : string Program.t)];
  [%expect {|
    (p (Apply x ((Apply b ()) (Apply c ((Apply d ()))))))
    (p (Apply y ((Apply b ()) (Apply c ((Apply d ()))))))
    (p (Apply z ((Apply b ()) (Apply c ((Apply d ()))))))
    (p (Apply a ((Apply x ()) (Apply c ((Apply d ()))))))
    (p (Apply a ((Apply y ()) (Apply c ((Apply d ()))))))
    (p (Apply a ((Apply z ()) (Apply c ((Apply d ()))))))
    (p (Apply a ((Apply b ()) (Apply x ((Apply d ()))))))
    (p (Apply a ((Apply b ()) (Apply y ((Apply d ()))))))
    (p (Apply a ((Apply b ()) (Apply z ((Apply d ()))))))
    (p (Apply a ((Apply b ()) (Apply c ((Apply x ()))))))
    (p (Apply a ((Apply b ()) (Apply c ((Apply y ()))))))
    (p (Apply a ((Apply b ()) (Apply c ((Apply z ())))))) |}]
