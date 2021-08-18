open Cad

let bounded params bench width =
  let ops = Bench.ops bench in

  let output = Bench.output bench in

  let search ?(view = ignore) _ center k =
    let check_program p =
      let v = Program.eval (Value.eval params) p in
      view v;
      if Value.equal v output then k p
    in
    try Tree_ball.Rename_insert_delete.ball (module Op) ops center width check_program
    with Program.Eval_error e -> raise @@ Program.Eval_error [%message (center : Op.t Program.t) (e : Sexp.t)]
  in
  search

let stochastic params bench =
  let ops = Bench.ops bench and output = Bench.output bench in
  let search ?(view = ignore) dist center k =
    try
      Tree_ball.Rename_insert_delete.stochastic ~k:5
        (module Op)
        ~score:(fun p -> 1.0 -. (dist output @@ Program.eval (Value.eval params) p))
        ops center
        (fun p _ ->
          let v = Program.eval (Value.eval params) p in
          view v;
          if [%compare.equal: Value.t] v output then k p)
    with Program.Eval_error e -> raise @@ Program.Eval_error [%message (center : Op.t Program.t) (e : Sexp.t)]
  in
  search

let leaf params bench =
  let module Op = Cad_op in
  let module Value = Cad_conc in
  let module F = Flat_program.Make (Op) in
  let eval = F.eval (Value.eval params) in
  let output = Bench.output bench in
  let search ?(view = ignore) _synth center k =
    try
      Tree_ball.Rename_leaves.stochastic
        (module Op)
        Cad_gen_pattern.rename center ~n:10
        ~score:(fun p -> Cad_conc.jaccard output @@ eval p)
        (fun p _ ->
          let v = eval p in
          view v;
          if [%compare.equal: Value.t] v output then k @@ F.to_program p)
    with Program.Eval_error e -> raise @@ Program.Eval_error [%message (center : Op.t Program.t) (e : Sexp.t)]
  in
  search
