open Cad

let full ?(n = 5) ?target ops ectx =
  let eval = Program.eval (Value.eval ectx) in
  let score = match target with Some t -> fun p -> Cad_conc.jaccard t @@ eval p | None -> Fun.const 1.0 in
  let search center k = Tree_ball.Rename_insert_delete.stochastic ~n (module Op) ~score ops center k in
  search

let leaf ?(n = 10) ?target ectx =
  let module Op = Cad_op in
  let module Value = Cad_conc in
  let module F = Flat_program.Make (Op) in
  let eval = F.eval (Value.eval ectx) in
  let score = match target with Some t -> fun p -> Cad_conc.jaccard t @@ eval p | None -> Fun.const 1.0 in
  let search center k =
    Tree_ball.Rename_leaves.stochastic
      (module Op)
      Cad_gen_pattern.rename center ~n ~score
      (fun ap d -> k (F.to_program ap) d)
  in
  search

module Pattern = struct
  type ('o, 'v) t = Apply of ('o * ('o, 'v) t list) | Var of 'v [@@deriving compare, sexp]

  let rec of_program Program.(Apply (op, args)) = Apply (op, List.map args ~f:of_program)

  let leaf_patterns ops =
    let module Op = Cad_op in
    let leaf_ops = List.filter ops ~f:(fun op -> Op.arity op = 0) in
    List.concat_map leaf_ops ~f:(fun op1 ->
        List.filter_map leaf_ops ~f:(fun op2 ->
            Option.some_if (not ([%compare.equal: Op.t] op1 op2)) (Apply (op1, []), Apply (op2, []))))

  let rename_patterns ops =
    let module Op = Cad_op in
    List.concat_map ops ~f:(fun op1 ->
        List.filter ops ~f:(fun op2 -> (not ([%compare.equal: Op.t] op1 op2)) && Op.arity op1 = Op.arity op2)
        |> List.map ~f:(fun op2 ->
               let args = List.init (Op.arity op1) ~f:(fun i -> Var i) in
               (Apply (op1, args), Apply (op2, args))))

  let match_root init bind p t =
    let bind ctx k v = Option.map ctx ~f:(fun ctx -> bind ctx k v) in
    let rec match_ ctx p t =
      match (p, t) with
      | Var v, t -> bind ctx v t
      | Apply (op, args), Program.Apply (op', args') ->
          if [%compare.equal: Op.t] op op' then
            List.fold2_exn args args' ~init:ctx ~f:(fun ctx p' t' -> match_ ctx p' t')
          else None
    in
    match_ (Some init) p t

  let rec match_all init bind p t k =
    Option.iter (match_root init bind p t) ~f:k;
    let (Apply (_, args)) = t in
    List.iter args ~f:(fun t' -> match_all init bind p t' k)

  let rec subst subst_var = function
    | Var v -> subst_var v
    | Apply (op, args) -> Program.Apply (op, List.map args ~f:(subst subst_var))

  let rewrite_root (lhs, rhs) t =
    Option.map
      (match_root (Map.empty (module Int)) (fun m k v -> Map.add_exn m ~key:k ~data:v) lhs t)
      ~f:(fun ctx -> subst (Map.find_exn ctx) rhs)

  let rec rewrite_all rule t k =
    Option.iter (rewrite_root rule t) ~f:k;
    let (Apply (op, args)) = t in
    List.iteri args ~f:(fun i t' ->
        rewrite_all rule t' (fun p -> k @@ Program.Apply (op, List.take args i @ (p :: List.drop args (i + 1)))))
end
