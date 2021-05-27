let zhang_sasha ~eq p p' =
  let open Tree_diff in
  let rec to_tree (Program.Apply (op, args)) =
    Tree.Branch (op, List.map ~f:to_tree args)
  in
  let t = to_tree p and t' = to_tree p' in
  Diff.zs_tree_dist0 t t' ~eq

module type Op_intf = sig
  type t [@@deriving compare, sexp]
end

let print_zhang_sasha_diff (type t) (module Op : Op_intf with type t = t)
    (p : Op.t Program.t) (p' : Op.t Program.t) =
  let open Tree_diff in
  let rec to_tree (Program.Apply (op, args)) =
    Tree.Branch (op, List.map ~f:to_tree args)
  in
  let t = to_tree p and t' = to_tree p' in
  Diff.diff t t' ~eq:[%compare.equal: Op.t]
  |> [%sexp_of: Op.t Diff.script] |> print_s
