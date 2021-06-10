module Arity_intf = struct
  module type S = sig
    type t

    val arity : t -> int
  end
end

module Rename_only = struct
  let rewrites (type op) (module Op : Arity_intf.S with type t = op)
      (ops : op list) t d f =
    let n = Program.size t in
    let rewrite = Array.create ~len:n None in

    (* Select ops that are valid at every program location *)
    let valid_ops = Array.create ~len:n [] in
    Program.iteri t ~f:(fun i op ->
        valid_ops.(i) <-
          List.filter ops ~f:(fun op' -> Op.arity op = Op.arity op'));

    for k = 1 to Int.min n d do
      Combinat.combinations (List.init n ~f:Fun.id) ~k @@ fun loc ->
      (* Only consider sequences of valid ops *)
      let selected_ops =
        Array.to_list loc |> List.map ~f:(fun l -> valid_ops.(l))
      in
      Combinat.sequences_restricted selected_ops @@ fun ops ->
      Array.fill rewrite ~pos:0 ~len:n None;
      Array.iteri loc ~f:(fun i l -> rewrite.(l) <- Some ops.(i));
      f rewrite
    done

  let ball (type op) (module Op : Arity_intf.S with type t = op) (ops : op list)
      t d f =
    rewrites (module Op) ops t d @@ fun rewrite ->
    f @@ Program.mapi t ~f:(fun i op -> Option.value rewrite.(i) ~default:op)

  let rec dist ~compare (Program.Apply (op, args)) (Program.Apply (op', args'))
      =
    let d = if compare op op' = 0 then 0.0 else 1.0 in
    if List.is_empty args && List.is_empty args' then d
    else if List.length args = List.length args' then
      d
      +. (List.map2_exn args args' ~f:(dist ~compare)
         |> List.sum (module Float) ~f:Fun.id)
    else Float.infinity

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
      let init =
        Program.(apply "a" ~args:[ apply "b"; apply "c" ~args:[ apply "d" ] ])
      in
      ball (module A) [ "x"; "y"; "z" ] init d @@ fun p ->
      print_s
        [%message
          (p : string Program.t)
            (dist ~compare:[%compare: string] p init : float)]
    in
    print_ball 1;
    [%expect
      {|
    ((p (Apply y ((Apply b ()) (Apply c ((Apply d ()))))))
     ("dist ~compare:([%compare : string]) p init" 1))
    ((p (Apply a ((Apply z ()) (Apply c ((Apply d ()))))))
     ("dist ~compare:([%compare : string]) p init" 1))
    ((p (Apply a ((Apply b ()) (Apply x ((Apply d ()))))))
     ("dist ~compare:([%compare : string]) p init" 1))
    ((p (Apply a ((Apply b ()) (Apply c ((Apply z ()))))))
     ("dist ~compare:([%compare : string]) p init" 1)) |}];
    print_ball 2;
    [%expect
      {|
    ((p (Apply y ((Apply b ()) (Apply c ((Apply d ()))))))
     ("dist ~compare:([%compare : string]) p init" 1))
    ((p (Apply a ((Apply z ()) (Apply c ((Apply d ()))))))
     ("dist ~compare:([%compare : string]) p init" 1))
    ((p (Apply a ((Apply b ()) (Apply x ((Apply d ()))))))
     ("dist ~compare:([%compare : string]) p init" 1))
    ((p (Apply a ((Apply b ()) (Apply c ((Apply z ()))))))
     ("dist ~compare:([%compare : string]) p init" 1))
    ((p (Apply y ((Apply z ()) (Apply c ((Apply d ()))))))
     ("dist ~compare:([%compare : string]) p init" 2))
    ((p (Apply y ((Apply b ()) (Apply x ((Apply d ()))))))
     ("dist ~compare:([%compare : string]) p init" 2))
    ((p (Apply a ((Apply z ()) (Apply x ((Apply d ()))))))
     ("dist ~compare:([%compare : string]) p init" 2))
    ((p (Apply y ((Apply b ()) (Apply c ((Apply z ()))))))
     ("dist ~compare:([%compare : string]) p init" 2))
    ((p (Apply a ((Apply z ()) (Apply c ((Apply z ()))))))
     ("dist ~compare:([%compare : string]) p init" 2))
    ((p (Apply a ((Apply b ()) (Apply x ((Apply z ()))))))
     ("dist ~compare:([%compare : string]) p init" 2)) |}]
end

module Rename_insert_delete = struct
  let ball (type op type_)
      (module Op : Op_intf.S with type t = op and type type_ = type_)
      (ops : op list) ?(n = 500) t d f =
    let module R = Rewrite.Make (Op) in
    let sample = R.sample ops in
    for _ = 0 to n do
      let _, t' = sample d t in
      f t'
    done

  let stochastic (type op type_)
      (module Op : Op_intf.S with type t = op and type type_ = type_)
      (ops : op list) ?(n = 5) ~score t f =
    let module R = Rewrite.Make (Op) in
    let sample = R.sample_single ops in
    let rec loop i t v =
      f t v;
      if i < n then
        let t' =
          let op = sample t in
          R.Value.eval () op [ t ]
        in
        let v' = score t' in
        let ratio = v' /. v in
        let accept = Random.float 1.0 in
        if Float.(accept < ratio) then loop (i + 1) t' v' else loop (i + 1) t v
    in
    loop 0 t (score t)

  let stochastic (type op type_)
      (module Op : Op_intf.S with type t = op and type type_ = type_)
      (ops : op list) ?n ?(k = 10000) ~score t f =
    let module R = Rewrite.Make (Op) in
    for _ = 0 to k do
      stochastic (module Op) ?n ops ~score t f
    done
end
