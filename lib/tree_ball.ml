module Arity_intf = struct
  module type S = sig
    type t

    val arity : t -> int
  end
end

let rewrites (type op) (module Op : Arity_intf.S with type t = op)
    (ops : op list) t d f =
  let n = Program.size t in
  let rewrite = Array.create ~len:n None in

  (* Select ops that are valid at every program location *)
  let valid_ops = Array.create ~len:n [] in
  Program.iteri t ~f:(fun i op ->
      valid_ops.(i) <-
        List.filter ops ~f:(fun op' -> Op.arity op = Op.arity op'));

  Combinat.combinations (List.init n ~f:Fun.id) ~k:d @@ fun loc ->
  (* Only consider sequences of valid ops *)
  let selected_ops =
    Array.to_list loc |> List.map ~f:(fun l -> valid_ops.(l))
  in
  Combinat.sequences_restricted selected_ops @@ fun ops ->
  Array.fill rewrite ~pos:0 ~len:n None;
  Array.iteri loc ~f:(fun i l -> rewrite.(l) <- Some ops.(i));
  f rewrite

let ball (type op) (module Op : Arity_intf.S with type t = op) (ops : op list) t
    d f =
  rewrites (module Op) ops t d @@ fun rewrite ->
  f @@ Program.mapi t ~f:(fun i op -> Option.value rewrite.(i) ~default:op)

let%expect_test "" =
  let module A = struct
    type t = string

    let arity = function
      | "x" | "c" -> 1
      | "y" | "a" -> 2
      | "z" | "b" | "d" -> 0
      | _ -> assert false
  end in
  let print_ball d =
    ball
      (module A)
      [ "x"; "y"; "z" ]
      Program.(apply "a" ~args:[ apply "b"; apply "c" ~args:[ apply "d" ] ])
      d
    @@ fun p -> print_s [%message (p : string Program.t)]
  in
  print_ball 1;
  [%expect
    {|
    (p (Apply y ((Apply b ()) (Apply c ((Apply d ()))))))
    (p (Apply a ((Apply z ()) (Apply c ((Apply d ()))))))
    (p (Apply a ((Apply b ()) (Apply x ((Apply d ()))))))
    (p (Apply a ((Apply b ()) (Apply c ((Apply z ())))))) |}];
  print_ball 2;
  [%expect
    {|
    (p (Apply y ((Apply z ()) (Apply c ((Apply d ()))))))
    (p (Apply y ((Apply b ()) (Apply x ((Apply d ()))))))
    (p (Apply a ((Apply z ()) (Apply x ((Apply d ()))))))
    (p (Apply y ((Apply b ()) (Apply c ((Apply z ()))))))
    (p (Apply a ((Apply z ()) (Apply c ((Apply z ()))))))
    (p (Apply a ((Apply b ()) (Apply x ((Apply z ())))))) |}]
