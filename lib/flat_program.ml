module Make (Op : Op_intf.S) = struct
  type t = Op.t array

  let rec write buf idx (Program.Apply (op, args)) =
    buf.(idx) <- op;
    write_args buf (idx + 1) args

  and write_args buf idx = function
    | [] -> idx
    | x :: xs -> write_args buf (write buf idx x) xs

  let rec read buf idx f =
    let op = buf.(idx) in
    let args, idx' = read_args buf (idx + 1) (Op.arity op) f in
    (f op args, idx')

  and read_args buf idx n_args f =
    if n_args = 0 then ([], idx)
    else
      let x, idx' = read buf idx f in
      let xs, idx'' = read_args buf idx' (n_args - 1) f in
      (x :: xs, idx'')

  let of_program p =
    let size = Program.size p in
    let (Apply (op, _)) = p in
    let buf = Array.create ~len:size op in
    ignore (write buf 0 p : int);
    buf

  let to_program buf =
    let p, _ = read buf 0 (fun op args -> Program.Apply (op, args)) in
    p

  let leaves buf =
    Array.filter_mapi buf ~f:(fun i op -> if Op.arity op = 0 then Some i else None)

  let eval eval_op buf = Tuple.T2.get1 @@ read buf 0 eval_op
end
