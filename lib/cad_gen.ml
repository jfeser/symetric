open Cad

let eval_program params = Program.eval (Value.eval params)

let random_int lo hi =
  [%test_pred: int * int] (fun (lo, hi) -> lo < hi) (lo, hi);
  lo + Random.int (hi - lo)

let pixels input conc =
  Cad_bench.points input |> List.map ~f:(Cad_conc.getp conc)

let has_empty_inter params p =
  let exception Empty_inter in
  let rec eval (Program.Apply (op, args)) =
    let args = List.map ~f:eval args in
    let v = Value.eval params op args in
    if
      [%compare.equal: Op.t] op Op.inter
      && pixels (Params.get params Cad_params.bench).input v
         |> List.for_all ~f:(fun x -> not x)
    then raise Empty_inter
    else v
  in
  try
    (eval p : Value.t) |> ignore;
    false
  with Empty_inter -> true

let non_trivial params p =
  let v = eval_program params p in
  let rec nilops = function
    | Program.Apply (op, []) -> [ op ]
    | Apply (_, args) -> List.concat_map ~f:nilops args
  in
  let v' =
    nilops p
    |> List.map ~f:(fun op -> Program.Apply (op, []))
    |> List.reduce_exn ~f:(fun p p' -> Program.Apply (Op.union, [ p; p' ]))
    |> eval_program params
  in
  not ([%compare.equal: Value.t] v v')

let check params p = (not (has_empty_inter params p)) && non_trivial params p

let random_op ~xmax ~ymax ~id = function
  | `Union -> Op.union
  | `Inter -> Op.inter
  | `Repl ->
      let v =
        List.random_element_exn
          [
            Vector2.{ x = 2.0; y = 2.0 };
            { x = -2.0; y = 2.0 };
            { x = 2.0; y = -2.0 };
            { x = -2.0; y = -2.0 };
          ]
      in
      Op.replicate ~id ~count:(Random.int_incl 1 4) ~v
  | `Circle ->
      let center =
        Vector2.
          {
            x = Random.int xmax |> Float.of_int;
            y = Random.int ymax |> Float.of_int;
          }
      in
      let radius = Random.int (Int.min xmax ymax / 2) |> Float.of_int in
      Op.circle ~id ~center ~radius
  | `Rect ->
      let lo_x = random_int 0 xmax in
      let lo_y = random_int 0 ymax in
      let hi_x = random_int lo_x xmax in
      let hi_y = random_int lo_y ymax in
      Op.rect ~id
        ~lo_left:{ x = Float.of_int lo_x; y = Float.of_int lo_y }
        ~hi_right:{ x = Float.of_int hi_x; y = Float.of_int hi_y }
