let eval_program params = Program.eval (Cad_conc.eval params)

let random_int lo hi =
  [%test_pred: int * int] (fun (lo, hi) -> lo < hi) (lo, hi);
  lo + Random.int (hi - lo)

let pixels input conc =
  Cad_bench.points input |> List.map ~f:(Cad_conc.getp conc)

let has_empty_inter params p =
  let exception Empty_inter in
  let rec eval (Program.Apply (op, args)) =
    let args = List.map ~f:eval args in
    let v = Cad_conc.eval params op args in
    if
      [%compare.equal: Cad_op.t] op Cad_op.inter
      && pixels (Params.get params Cad_params.bench).input v
         |> List.for_all ~f:(fun x -> not x)
    then raise Empty_inter
    else v
  in
  try
    (eval p : Cad_conc.t) |> ignore;
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
    |> List.reduce_exn ~f:(fun p p' -> Program.Apply (Cad_op.union, [ p; p' ]))
    |> eval_program params
  in
  not ([%compare.equal: Cad_conc.t] v v')

let check params p = (not (has_empty_inter params p)) && non_trivial params p

module Op_kind = struct
  module T = struct
    type t = Circle | Rect | Union | Inter | Repl [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

include struct
  open Dumb_params

  let spec = Spec.create ()

  let ops =
    Spec.add spec
    @@ Param.ids ~name:"ops" ~doc:" operators to generate"
         (module Op_kind)
         [
           ("circle", Circle);
           ("rectangle", Rect);
           ("union", Union);
           ("intersection", Inter);
           ("replicate", Repl);
         ]

  let xmax = Spec.add spec @@ Param.int ~name:"xmax" ~doc:" x width" ()

  let ymax = Spec.add spec @@ Param.int ~name:"ymax" ~doc:" y width" ()

  let spec = Spec.union [ spec; Cad_conc.spec ]
end

let random_ops params =
  let xmax = Params.get params xmax and ymax = Params.get params ymax in
  List.mapi (Params.get params ops) ~f:(fun id -> function
    | Op_kind.Union -> Cad_op.union
    | Inter -> Cad_op.inter
    | Repl ->
        let v =
          List.random_element_exn
            [
              Vector2.{ x = 2.0; y = 2.0 };
              { x = -2.0; y = 2.0 };
              { x = 2.0; y = -2.0 };
              { x = -2.0; y = -2.0 };
            ]
        in
        Cad_op.replicate ~id ~count:(Random.int_incl 1 4) ~v
    | Circle ->
        let center =
          Vector2.
            {
              x = Random.int xmax |> Float.of_int;
              y = Random.int ymax |> Float.of_int;
            }
        in
        let radius = Random.int (Int.min xmax ymax / 2) |> Float.of_int in
        Cad_op.circle ~id ~center ~radius
    | Rect ->
        let lo_x = random_int 0 xmax in
        let lo_y = random_int 0 ymax in
        let hi_x = random_int lo_x xmax in
        let hi_y = random_int lo_y ymax in
        Cad_op.rect ~id
          ~lo_left:{ x = Float.of_int lo_x; y = Float.of_int lo_y }
          ~hi_right:{ x = Float.of_int hi_x; y = Float.of_int hi_y })

let gen_ops = ref None

let random_program params size =
  let ops =
    match !gen_ops with
    | None ->
        let ops = random_ops params in
        gen_ops := Some ops;
        ops
    | Some ops -> ops
  in
  let params =
    let xmax = Params.get params xmax and ymax = Params.get params ymax in
    Dumb_params.set params Cad_params.bench
      Cad_bench.
        {
          ops = [];
          input = { xmax; ymax };
          output = Cad_conc.dummy;
          solution = None;
          filename = None;
        }
  in

  let open Option.Let_syntax in
  let nilops = List.filter ops ~f:(fun op -> Cad_op.arity op = 0)
  and unops = List.filter ops ~f:(fun op -> Cad_op.arity op = 1)
  and binops = List.filter ops ~f:(fun op -> Cad_op.arity op = 2) in
  let rec random_unop op size =
    let%map p = random_tree (size - 1) in
    Program.Apply (op, [ p ])
  and random_binop op size =
    Combinat.(compositions ~n:(size - 1) ~k:2 |> to_list)
    |> List.permute
    |> List.find_map ~f:(fun ss ->
           let s = ss.(0) and s' = ss.(1) in
           let%bind p = random_tree s and p' = random_tree s' in
           return @@ Program.Apply (op, [ p; p' ]))
  and random_tree size =
    [%test_pred: int] (fun size -> size > 0) size;
    if size = 1 then
      let op = List.random_element_exn nilops in
      Some (Program.Apply (op, []))
    else if size = 2 then random_unop (List.random_element_exn unops) size
    else
      let op = List.random_element_exn (unops @ binops) in
      let arity = Cad_op.arity op in
      if arity = 1 then random_unop op size
      else if arity = 2 then random_binop op size
      else failwith ""
  in
  let prog = Option.value_exn (random_tree size) in
  if check params prog then return (prog, ops) else None

(* let random_program params size =
 *   let open Option.Let_syntax in
 *   let ops =
 *     match !gen_ops with
 *     | None ->
 *         let ops = random_ops params in
 *         gen_ops := Some ops;
 *         ops
 *     | Some ops -> ops
 *   in
 *   let params =
 *     let xmax = Params.get params xmax and ymax = Params.get params ymax in
 *     Dumb_params.set params Cad_params.bench
 *       Cad_bench.
 *         {
 *           ops = [];
 *           input = { xmax; ymax };
 *           output = Cad_conc.dummy;
 *           solution = None;
 *           filename = None;
 *         }
 *   in
 * 
 *   let random_op type_ min_args max_args =
 *     List.filter ops ~f:(fun op ->
 *         let arity = Cad_op.arity op in
 *         [%compare.equal: Cad_type.t] type_ (Cad_op.ret_type op)
 *         && min_args <= arity && arity <= max_args)
 *     |> List.random_element
 *   in
 * 
 *   let random_args random_tree op size =
 *     Combinat.(compositions ~n:size ~k:(Cad_op.arity op) |> to_list)
 *     |> List.permute
 *     |> List.find_map ~f:(fun ss ->
 *            List.map2_exn (Cad_op.args_type op) (Array.to_list ss) ~f:random_tree
 *            |> Option.all)
 *   in
 * 
 *   let rec random_tree type_ size =
 *     [%test_pred: int] (fun size -> size > 0) size;
 * 
 *     let%bind op =
 *       random_op type_
 *         (if size = 1 then 0 else 1)
 *         (if size = 1 then 0 else size - 1)
 *     in
 *     let%bind args = random_args random_tree op (size - 1) in
 *     let p = Program.Apply (op, args) in
 *     if check params p then return p else None
 *   in
 * 
 *   Option.map (random_tree Cad_type.output size) ~f:(fun prog -> (prog, ops)) *)

let to_bench params ops solution output =
  let xmax = Params.get params xmax and ymax = Params.get params ymax in
  {
    Cad_bench.ops;
    input = { xmax; ymax };
    output;
    solution = Some solution;
    filename = None;
  }
