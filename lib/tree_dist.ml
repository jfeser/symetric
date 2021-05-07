let zhang_sasha ~eq p p' =
  let open Tree_diff in
  let rec to_tree (Program.Apply (op, args)) =
    Tree.Branch (op, List.map ~f:to_tree args)
  in
  let t = to_tree p and t' = to_tree p' in
  Diff.zs_tree_dist0 t t' ~eq
