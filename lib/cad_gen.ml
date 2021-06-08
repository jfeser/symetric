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

let to_bench params ops solution output =
  let xmax = Params.get params xmax and ymax = Params.get params ymax in
  {
    Cad_bench.ops;
    input = { xmax; ymax };
    output;
    solution = Some solution;
    filename = None;
  }
