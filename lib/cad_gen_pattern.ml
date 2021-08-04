let eval_program params = Program.eval (Cad_conc.eval params)

let random_int lo hi =
  [%test_pred: int * int] (fun (lo, hi) -> lo < hi) (lo, hi);
  lo + Random.int (hi - lo)

let pixels input conc = Cad_bench.points input |> List.map ~f:(Cad_conc.getp conc)

module B = Baseline.Make (Cad)

class filler params =
  object (self)
    inherit B.synthesizer params

    method build_search_state =
      for cost = 0 to max_cost do
        self#fill cost
      done;
      search_state
  end

include struct
  open Dumb_params

  let spec = Spec.inherit_ Baseline.spec "cad-gen"

  let xmax = Spec.add spec @@ Param.int ~name:"xmax" ~doc:" x width" ()

  let ymax = Spec.add spec @@ Param.int ~name:"ymax" ~doc:" y width" ()

  let centers = Spec.add spec @@ Param.int ~name:"centers" ~doc:" number of centers" ()

  let output_filename = Spec.add spec @@ Param.string ~name:"output" ~doc:" output filename" ()

  let spec = Spec.union [ spec; Cad_conc.spec ]
end

let move_to x y s =
  let op' =
    match Cad_op.value s with
    | Cad_op.Circle c -> Cad_op.Circle { c with center = Vector2.{ x; y } }
    | Rect r ->
        let w = r.hi_right.x -. r.lo_left.x and h = r.hi_right.y -. r.lo_left.y in
        let lo_left = Vector2.{ x = x -. (w /. 2.0); y = y -. (h /. 2.0) }
        and hi_right = Vector2.{ x = x +. (w /. 2.0); y = y +. (h /. 2.0) } in
        Rect { r with lo_left; hi_right }
    | op -> raise_s [%message "cannot move" (op : Cad_op.op)]
  in
  Cad_op.create op'

let grid xmax ymax n shape =
  List.range 0 n
  |> List.map ~f:(fun x ->
         List.range 0 n
         |> List.map ~f:(fun y ->
                let x = Float.(round @@ (of_int x * of_int xmax / of_int n))
                and y = Float.(round @@ (of_int y * of_int ymax / of_int n)) in
                move_to x y shape))
  |> List.concat

let shapes xmax ymax =
  let small_circles = grid xmax ymax 3 (Cad_op.circle ~id:0 ~center:Vector2.zero ~radius:3.0) in
  let small_rects = grid xmax ymax 3 (Cad_op.rect ~id:0 ~lo_left:Vector2.zero ~hi_right:Vector2.{ x = 3.0; y = 3.0 }) in
  small_circles @ small_rects

let replicates =
  let step = 5.0 in
  [ Vector2.{ x = step; y = step }; { x = -.step; y = step }; { x = step; y = -.step }; { x = -.step; y = -.step } ]
  |> List.map ~f:(fun v -> Cad_op.replicate ~id:0 ~count:4 ~v)

let ops xmax ymax = shapes xmax ymax @ replicates @ [ Cad_op.union; Cad_op.inter ]

let checkpoint states fn =
  let open Owl in
  let features, classes = Hashtbl.to_alist states |> List.unzip in
  let to_features v = Arr.expand (Cad_conc.to_ndarray v) 2 in
  Npy.write (Mat.of_rows @@ Array.of_list @@ List.map features ~f:to_features) @@ sprintf "%s-features.npy" fn;
  Npy.write (Arr.of_array (Array.of_list @@ List.map classes ~f:Float.of_int) [| List.length classes |])
  @@ sprintf "%s-classes.npy" fn

let mk_dataset params =
  let size = Params.get params Baseline.max_cost and xmax = Params.get params xmax and ymax = Params.get params ymax in
  let all_shapes = shapes xmax ymax and other_ops = replicates @ [ Cad_op.union; Cad_op.inter ] in
  let all_states = Hashtbl.create (module Cad_conc) in

  let add_state c s = Hashtbl.set all_states ~key:c ~data:s in

  for class_ = 0 to Params.get params centers do
    let ops = List.take (List.permute all_shapes) 4 @ other_ops in
    let params =
      Dumb_params.set params Cad_params.bench
        { ops; input = { xmax; ymax }; output = Cad_conc.dummy; solution = None; filename = None }
    in

    let filler = new filler params in
    let search_state = filler#build_search_state in

    let output = List.hd_exn @@ List.permute @@ B.Search_state.search ~cost:size ~type_:Cad_type.output search_state in
    add_state output class_;

    for _ = 0 to 10 do
      let center = B.Search_state.random_program_exn search_state output in
      Tree_ball.Rename_insert_delete.ball ~n:100
        (module Cad_op)
        ops center 2
        (fun p' ->
          let output' = eval_program params p' in
          if
            Option.map (B.Search_state.cost_of search_state output') ~f:(fun c -> c >= size)
            |> Option.value ~default:true
          then add_state output' class_)
    done;

    checkpoint all_states @@ Params.get params output_filename;
    Fmt.epr "Finished %d classes\n%!" (class_ + 1)
  done
