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

let xmax = 30

let ymax = 30

let tall_rects = grid xmax ymax 3 (Cad_op.rect ~id:0 ~lo_left:Vector2.zero ~hi_right:Vector2.{ x = 1.0; y = 3.0 })

let wide_rects = grid xmax ymax 3 (Cad_op.rect ~id:0 ~lo_left:Vector2.zero ~hi_right:Vector2.{ x = 3.0; y = 1.0 })

let shapes = tall_rects @ wide_rects

let feat_of_shape =
  List.mapi tall_rects ~f:(fun i op -> (op, (i, 0))) @ List.mapi wide_rects ~f:(fun i op -> (op, (i, 1)))

let shape_of_feat = List.map feat_of_shape ~f:Tuple.T2.swap

let feat_of_shape = Hashtbl.of_alist_exn (module Cad_op) feat_of_shape

module I2 = struct
  module T = struct
    type t = int * int [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)
end

let shape_of_feat = Hashtbl.of_alist_exn (module I2) shape_of_feat

let all_renames op =
  let p, _ = Hashtbl.find_exn feat_of_shape op in
  List.init 2 ~f:(fun f -> Hashtbl.find_exn shape_of_feat (p, f))

let rename op =
  let p, _ = Hashtbl.find_exn feat_of_shape op in
  let k' = Random.int 2 in
  Hashtbl.find_exn shape_of_feat (p, k')

let replicates =
  let step = 5.0 in
  [ Vector2.{ x = step; y = step }; { x = -.step; y = step }; { x = step; y = -.step }; { x = -.step; y = -.step } ]
  |> List.map ~f:(fun v -> Cad_op.replicate ~id:0 ~count:4 ~v)

let ops = shapes @ replicates @ [ Cad_op.union; Cad_op.inter ]

let checkpoint states fn =
  let open Owl in
  let features, classes =
    Hashtbl.to_alist states
    |> List.concat_map ~f:(fun (feat, classes) -> Set.to_list classes |> List.map ~f:(fun c -> (feat, Float.of_int c)))
    |> List.unzip
  in

  let to_features v = Arr.expand (Cad_conc.to_ndarray v) 2 in
  Npy.write (Mat.of_rows @@ Array.of_list @@ List.map features ~f:to_features) @@ sprintf "%s-features.npy" fn;
  Npy.write (Arr.of_array (Array.of_list classes) [| List.length classes |]) @@ sprintf "%s-classes.npy" fn

let print_overlap states =
  let n_states = Hashtbl.length states in
  let n_overlap = Hashtbl.count states ~f:(fun l -> Set.length l > 1) in
  Fmt.epr "Overlapping %d/%d states\n%!" n_overlap n_states

let print_example of_class =
  let n_classes = Hashtbl.length of_class in
  let cls = Random.int n_classes in
  match Hashtbl.find_exn of_class cls with
  | center :: close :: _ -> (
      let next_class = (cls + 1) mod n_classes in
      if next_class <> cls then
        match Hashtbl.find_exn of_class next_class with
        | far :: _ ->
            Fmt.epr "Center:\n%a\n\nClose:\n%a\n\nFar:\n%a\n\n%!" Cad_conc.pprint center Cad_conc.pprint close
              Cad_conc.pprint far
        | _ -> ())
  | _ -> ()

let mk_dataset params =
  let max_cost = Params.get params Baseline.max_cost in
  let other_ops = replicates @ [ Cad_op.union; Cad_op.inter ] in
  let all_states = Hashtbl.create (module Cad_conc) in
  let of_class = Hashtbl.create (module Int) in

  let add_state c s =
    Hashtbl.update all_states c ~f:(function
      | Some ss ->
          if not (Set.mem ss s) then Hashtbl.add_multi of_class ~key:s ~data:c;
          Set.add ss s
      | None ->
          Hashtbl.add_multi of_class ~key:s ~data:c;
          Set.singleton (module Int) s)
  in

  let module F = Flat_program.Make (Cad_op) in
  let n_classes = Params.get params centers in
  for class_ = 0 to n_classes - 1 do
    let ops = List.take (List.permute shapes) 4 @ other_ops in
    let params =
      Dumb_params.set params Cad_params.bench
        { ops; input = { xmax; ymax }; output = Cad_conc.dummy; solution = None; filename = None }
    in

    let filler = new filler params in
    let search_state = filler#build_search_state in

    for cost = 3 to max_cost do
      let outputs = List.permute @@ B.Search_state.search ~cost ~type_:Cad_type.output search_state in
      List.take outputs 10
      |> List.iter ~f:(fun output ->
             add_state output class_;
             for _ = 0 to 10 do
               let center = B.Search_state.random_program_exn search_state output in
               Tree_ball.Rename_leaves.sample ~n:100 ~d:2 (module Cad_op) rename center @@ fun p' ->
               let output' = F.eval (Cad_conc.eval params) p' in
               if
                 Option.map (B.Search_state.cost_of search_state output') ~f:(fun c -> c >= cost)
                 |> Option.value ~default:true
               then add_state output' class_
             done)
    done;

    print_overlap all_states;
    print_example of_class;
    checkpoint all_states @@ Params.get params output_filename;
    Fmt.epr "Finished %d classes\n%!" (class_ + 1)
  done

let mk_bench params =
  let size = Params.get params Baseline.max_cost in
  let output_fn = Params.get params output_filename in
  let other_ops = replicates @ [ Cad_op.union; Cad_op.inter ] in
  let all_states = Hashtbl.create (module Cad_conc) in

  let add_state c s = Hashtbl.set all_states ~key:c ~data:s in

  for class_ = 0 to Params.get params centers do
    let shape_ops = List.take (List.permute shapes) 4 in
    let ops = shape_ops @ other_ops in
    let params =
      Dumb_params.set params Cad_params.bench
        { ops; input = { xmax; ymax }; output = Cad_conc.dummy; solution = None; filename = None }
    in

    let filler = new filler params in
    let search_state = filler#build_search_state in

    let output = List.hd_exn @@ List.permute @@ B.Search_state.search ~cost:size ~type_:Cad_type.output search_state in
    let solution = B.Search_state.random_program_exn search_state output in
    if not (Hashtbl.mem all_states output) then (
      add_state output class_;
      let solution_ops = List.concat_map shape_ops ~f:all_renames @ other_ops in
      let bench =
        { Cad.Bench.ops = solution_ops; input = { xmax; ymax }; output; solution = Some solution; filename = None }
      in
      Cad.Bench.save [%string "%{output_fn}/scene_%{class_#Int}.sexp"] bench)
  done
