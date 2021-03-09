open! Core
open Staged_synth
open Cad_op
open Cad_bench

let circle_inter =
  let c1 = Circle { id = 0; center = { x = 2.0; y = 2.0 }; radius = 2.0 } in
  let c2 = Circle { id = 1; center = { x = 3.0; y = 2.0 }; radius = 2.0 } in
  let prog = Program.Apply (Inter, [ Apply (c1, []); Apply (c2, []) ]) in
  let ops = [ Inter; Union; c1; c2 ] in
  (prog, ops)

let circle_and_rect =
  let c1 = Circle { id = 0; center = { x = 2.0; y = 2.0 }; radius = 2.0 } in
  let r1 =
    Rect
      {
        id = 0;
        lo_left = { x = 0.0; y = 0.0 };
        hi_right = { x = 4.0; y = 4.0 };
      }
  in
  let prog = Program.Apply (c1, []) in
  let ops = [ Inter; Union; c1; r1 ] in
  (prog, ops)

let circles_and_rects n =
  let circs =
    List.init n ~f:(fun i ->
        Circle
          {
            id = i;
            center = { x = Float.of_int @@ ((i * 2) + 1); y = 1.0 };
            radius = 1.0;
          })
  in
  let r1 =
    Rect
      {
        id = 0;
        lo_left = { x = 0.0; y = 0.0 };
        hi_right = { x = 2.0 *. Float.of_int n; y = 2.0 };
      }
  in
  let prog =
    let open Program in
    List.map circs ~f:(fun c -> Apply (c, []))
    |> List.reduce ~f:(fun c c' -> Apply (Union, [ c; c' ]))
  in
  let ops = [ Inter; Union; r1 ] @ circs in
  (Option.value_exn prog, ops)

let pp_iters_fixed =
  let pp fmt n = Format.fprintf fmt "%10Ld" n in
  (pp, 10)

let iters f =
  let pp, width = pp_iters_fixed in
  f ~width pp

let no_total_counter ?(mode = `ASCII) ?message ?pp ?width
    ?(sampling_interval = 1) () =
  let open Progress in
  let open Segment in
  let box =
    match width with
    | Some width -> box_fixed width
    | None -> box_winsize ~fallback:80
  in
  list
    ( (Option.map message ~f:const |> Option.to_list)
    @ (Option.map pp ~f:(fun f -> f of_pp) |> Option.to_list) )
  |> box |> periodic sampling_interval |> accumulator Int64.( + ) 0L
  |> make ~init:0L

let bar msg = no_total_counter ~message:msg ~pp:iters ()

let tbar msg total = Progress.counter ~total ~message:msg ~pp:iters ()

let sequence_progress bar =
  Sequence.unfold_with ~init:() ~f:(fun () x ->
      bar 1L;
      Yield (x, ()))

let to_sexp ~xmax ~ymax (prog, ops) =
  let input = { xmax; ymax } in
  let bench =
    Cad_bench.
      { ops = []; input; output = Map.empty (module Vector2); solution = None }
  in
  let params = Params.create bench Cad_params.{ concrete = false } in
  let conc = Program.eval (Cad_conc.eval params) prog in

  let output =
    Cad_bench.points input
    |> List.map ~f:(fun p -> if Map.find_exn conc p then 1 else 0)
  in

  [%sexp_of: Cad_bench.Serial.t]
    Cad_bench.Serial.{ ops; input; output; solution = Some prog }

let dump ~xmax ~ymax x = print_s @@ to_sexp ~xmax ~ymax x

let random_int lo hi =
  [%test_pred: int * int] (fun (lo, hi) -> lo < hi) (lo, hi);
  lo + Random.int (hi - lo)

let has_empty_inter params p =
  let exception Empty_inter in
  let rec eval (Program.Apply (op, args)) =
    let args = List.map ~f:eval args in
    let v = Cad_conc.eval params op args in
    if [%compare.equal: Cad_op.t] op Inter && Map.for_all v ~f:(fun x -> not x)
    then raise Empty_inter
    else v
  in
  try
    (eval p : Cad_conc.t) |> ignore;
    false
  with Empty_inter -> true

let has_noop params p =
  let exception Noop in
  let rec eval (Program.Apply (op, args)) =
    let args = List.map ~f:eval args in
    let v = Cad_conc.eval params op args in
    if List.mem args v ~equal:[%compare.equal: Cad_conc.t] then raise Noop
    else v
  in
  try
    (eval p : Cad_conc.t) |> ignore;
    false
  with Noop -> true

let non_trivial params p =
  let v = Cad_conc.eval_program params p in
  let rec nilops = function
    | Program.Apply (op, []) -> [ op ]
    | Apply (_, args) -> List.concat_map ~f:nilops args
  in
  let v' =
    nilops p
    |> List.map ~f:(fun op -> Program.Apply (op, []))
    |> List.reduce_exn ~f:(fun p p' -> Program.Apply (Union, [ p; p' ]))
    |> Cad_conc.eval_program params
  in
  not ([%compare.equal: Cad_conc.t] v v')

let irreducible params p =
  let values = Hashtbl.create (module Cad_conc) in
  let rec eval (Program.Apply (op, args) as p) =
    let args = List.map ~f:eval args in
    let out = Cad_conc.eval params op args in
    Hashtbl.update values out ~f:(fun ps -> p :: Option.value ps ~default:[]);
    out
  in
  (eval p : Cad_conc.t) |> ignore;
  not @@ Hashtbl.existsi values ~f:(fun ~key:_ ~data:ps -> List.length ps > 1)

let take_while_with_state ~init ~f s =
  Sequence.unfold_with s ~init ~f:(fun st x ->
      match f st x with Some st' -> Yield (x, st') | None -> Done)

let random ~xmax ~ymax ~size ~nprim ~n =
  let params =
    Params.create
      Cad_bench.
        {
          ops = [];
          input = { xmax; ymax };
          output = Map.empty (module Vector2);
          solution = None;
        }
      Cad_params.{ concrete = false }
  in

  let random_program size =
    let nilops =
      List.init nprim ~f:(fun i ->
          match List.random_element_exn [ `Circle; `Rect ] with
          | `Circle ->
              Circle
                {
                  id = i;
                  center =
                    {
                      x = Random.int xmax |> Float.of_int;
                      y = Random.int ymax |> Float.of_int;
                    };
                  radius = Random.int (Int.min xmax ymax / 2) |> Float.of_int;
                }
          | `Rect ->
              let lo_x = random_int 0 xmax in
              let lo_y = random_int 0 ymax in
              let hi_x = random_int lo_x xmax in
              let hi_y = random_int lo_y ymax in
              Rect
                {
                  id = i;
                  lo_left = { x = Float.of_int lo_x; y = Float.of_int lo_y };
                  hi_right = { x = Float.of_int hi_x; y = Float.of_int hi_y };
                })
    in
    let binops = [ Union; Inter ] in

    let exception Bad_size in
    let rec random_tree size =
      if size <= 0 then raise Bad_size
      else if size = 1 then
        let op = List.random_element_exn nilops in
        Program.Apply (op, [])
      else if size = 2 then raise Bad_size
      else
        let op = List.random_element_exn binops in
        let s = random_int 1 (size - 1) in
        let s' = size - 1 - s in
        let p = random_tree s and p' = random_tree s' in
        Apply (op, [ p; p' ])
    in
    try
      let prog = random_tree size and ops = nilops @ binops in
      Some (prog, ops)
    with Bad_size -> None
  in
  Progress.(
    with_reporters
      (bar "samples" / bar "programs" / tbar "unique_programs" (Int64.of_int n)))
  @@ fun ((samples, programs), unique_programs) ->
  Sequence.unfold ~init:() ~f:(fun () -> Some (random_program size, ()))
  |> sequence_progress samples
  |> Sequence.filter_map ~f:Fun.id
  |> sequence_progress programs
  |> Sequence.filter ~f:(fun (p, _) ->
         (not (has_empty_inter params p))
         && (not (has_noop params p))
         && non_trivial params p && irreducible params p)
  |> take_while_with_state
       ~init:(Set.empty (module Cad_conc))
       ~f:(fun seen (prog, _) ->
         if Set.length seen < n then
           let out = Cad_conc.eval_program params prog in
           if Set.mem seen out then Some seen else Some (Set.add seen out)
         else None)
  |> sequence_progress unique_programs
  |> Sequence.map ~f:(to_sexp ~xmax ~ymax)
  |> Sequence.force_eagerly

let circles_and_rects_unsat =
  let ([ c1; c2; c3 ] as circs) =
    List.init 3 ~f:(fun i ->
        Circle
          {
            id = i;
            center = { x = Float.of_int @@ ((i * 2) + 1); y = 1.0 };
            radius = 1.0;
          })
  in
  let r1 =
    Rect
      {
        id = 0;
        lo_left = { x = 0.0; y = 0.0 };
        hi_right = { x = 6.0; y = 2.0 };
      }
  in
  let prog = Program.(Apply (r1, [])) in

  let ops = [ Inter; Union ] @ circs in
  (prog, ops)

let dumps ~dir ~prefix seq =
  Sequence.iteri seq ~f:(fun i sexp ->
      Out_channel.with_file [%string "%{dir}/%{prefix}_%{i#Int}.sexp"]
        ~f:(fun ch -> Sexp.output_hum ch sexp))

let () =
  Random.init 0;
  random ~xmax:30 ~ymax:30 ~size:9 ~nprim:5 ~n:100
  |> dumps ~dir:"bench/cad2/random_size_9" ~prefix:"scene"
