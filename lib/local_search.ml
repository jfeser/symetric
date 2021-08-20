open Cad

let full ?(n = 5) ?target params =
  let ops = Bench.ops @@ Params.get params bench in
  let eval = Program.eval (Value.eval params) in
  let score = match target with Some t -> fun p -> Cad_conc.jaccard t @@ eval p | None -> Fun.const 1.0 in
  let search center k = Tree_ball.Rename_insert_delete.stochastic ~n (module Op) ~score ops center k in
  search

let leaf ?(n = 10) ?target params =
  let module Op = Cad_op in
  let module Value = Cad_conc in
  let module F = Flat_program.Make (Op) in
  let eval = F.eval (Value.eval params) in
  let score = match target with Some t -> fun p -> Cad_conc.jaccard t (eval p) | None -> Fun.const 1.0 in
  let search center k =
    Tree_ball.Rename_leaves.stochastic
      (module Op)
      Cad_gen_pattern.rename center ~n ~score
      (fun ap d -> k (F.to_program ap) d)
  in
  search
