open! Core
open Staged_synth
open Cad_op
open Cad_bench

let rec union_many = function
  | [] -> failwith "unexpected empty list"
  | [ x ] -> x
  | x :: xs -> Program.Apply (Union, [ x; union_many xs ])

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

let no_total_counter ?message ?pp ?width ?(sampling_interval = 1) () =
  let open Progress in
  let open Segment in
  let box =
    match width with
    | Some width -> box_fixed width
    | None -> box_winsize ~fallback:80
  in
  list
    ((Option.map message ~f:const |> Option.to_list)
    @ (Option.map pp ~f:(fun f -> f of_pp) |> Option.to_list))
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
      {
        ops = [];
        input;
        output =
          Cad_conc.
            {
              xlen = -1;
              ylen = -1;
              pixels = Bitarray.init 0 ~f:(fun _ -> false);
            };
        solution = None;
        filename = None;
      }
  in
  let params = Params.create bench Cad_params.{ concrete = false } in
  let conc = Program.eval (Cad_conc.eval params) prog in

  let output =
    Bitarray.to_list conc.pixels |> List.map ~f:(fun p -> if p then 1 else 0)
  in

  [%sexp_of: Cad_bench.Serial.t]
    Cad_bench.Serial.{ ops; input; output; solution = Some prog }

let random_int lo hi =
  [%test_pred: int * int] (fun (lo, hi) -> lo < hi) (lo, hi);
  lo + Random.int (hi - lo)

let has_empty_inter params p =
  let exception Empty_inter in
  let rec eval (Program.Apply (op, args)) =
    let args = List.map ~f:eval args in
    let v = Cad_conc.eval params op args in
    if [%compare.equal: Cad_op.t] op Inter && Bitarray.(all @@ not v.pixels)
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

let random_op ~xmax ~ymax ~id = function
  | `Union -> Union
  | `Inter -> Inter
  | `Repl ->
      Replicate
        {
          id;
          count = Random.int_incl 1 4;
          v =
            List.random_element_exn
              [
                Vector2.{ x = 2.0; y = 2.0 };
                { x = -2.0; y = 2.0 };
                { x = 2.0; y = -2.0 };
                { x = -2.0; y = -2.0 };
              ];
        }
  | `Circle ->
      Circle
        {
          id;
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
          id;
          lo_left = { x = Float.of_int lo_x; y = Float.of_int lo_y };
          hi_right = { x = Float.of_int hi_x; y = Float.of_int hi_y };
        }

(** 
@param xmax canvas x length
@param ymax canvas y length
@param size program size
@param nprim number of primitives available
@param n number of programs to generate 
@param k callback with program sequence *)
let random ~xmax ~ymax ~size ~n ~ops k =
  let params =
    Params.create
      Cad_bench.
        {
          ops = [];
          input = { xmax; ymax };
          output =
            Cad_conc.
              {
                xlen = -1;
                ylen = -1;
                pixels = Bitarray.init 0 ~f:(fun _ -> false);
              };
          solution = None;
          filename = None;
        }
      Cad_params.{ concrete = false }
  in

  let random_program size =
    let open Option.Let_syntax in
    let ops = List.mapi ops ~f:(fun id op -> random_op ~xmax ~ymax ~id op) in
    let nilops =
      List.filter ops ~f:(function Circle _ | Rect _ -> true | _ -> false)
    and unops = List.filter ops ~f:(function Replicate _ -> true | _ -> false)
    and binops =
      List.filter ops ~f:(function Union | Inter -> true | _ -> false)
    in
    let rec random_unop op size =
      let%map p = random_tree (size - 1) in
      Program.Apply (op, [ p ])
    and random_binop op size =
      Combinat.(Composition.(create ~n:(size - 1) ~k:2 |> to_list))
      |> List.permute
      |> List.find_map ~f:(fun ss ->
             let s = Combinat.Int_array.get ss 0
             and s' = Combinat.Int_array.get ss 1 in
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
    (prog, ops)
  in
  Progress.(
    with_reporters
      (bar "samples" / bar "non-trivial programs" / bar "irreducible programs"
      / tbar "unique programs" (Int64.of_int n)))
  @@ fun (((samples, non_trivial_progs), irreducible_progs), unique_progs) ->
  Sequence.unfold ~init:() ~f:(fun () -> Some (random_program size, ()))
  |> sequence_progress samples
  |> Sequence.filter ~f:(fun (p, _) ->
         (not (has_empty_inter params p))
         && (not (has_noop params p))
         && non_trivial params p)
  |> sequence_progress non_trivial_progs
  |> Sequence.filter ~f:(fun (p, _) -> irreducible params p)
  |> sequence_progress irreducible_progs
  |> take_while_with_state
       ~init:(Set.empty (module Cad_conc))
       ~f:(fun seen (prog, _) ->
         if Set.length seen < n then
           let out = Cad_conc.eval_program params prog in
           if Set.mem seen out then Some seen else Some (Set.add seen out)
         else None)
  |> sequence_progress unique_progs
  |> Sequence.map ~f:(to_sexp ~xmax ~ymax)
  |> Sequence.mapi ~f:(fun i sexp -> ([%string "scene_%{i#Int}.sexp"], sexp))
  |> k

let circles_and_rects_unsat =
  let circs =
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

let crosses n =
  let shapes =
    List.init n ~f:(fun i ->
        let j = Float.of_int i in
        let vert =
          Rect
            {
              id = i;
              lo_left = { x = j +. 1.0; y = 0.0 };
              hi_right = { x = j +. 3.0; y = 4.0 };
            }
        in
        let horiz =
          Rect
            {
              id = n + i;
              lo_left = { x = j; y = 1.0 };
              hi_right = { x = j +. 4.0; y = 3.0 };
            }
        in
        [ vert; horiz ])
    |> List.concat
  in
  let prog =
    let lhs = List.take shapes 2 in
    let rhs = List.take (List.rev shapes) 2 in
    union_many @@ List.map (lhs @ rhs) ~f:Program.apply
  in
  let ops = Union :: shapes in
  (sprintf "crosses_%d" n, to_sexp ~xmax:(4 * n) ~ymax:4 (prog, ops))

let dumps ~dir seq =
  Unix.mkdir_p dir;
  Sequence.iter seq ~f:(fun (fn, sexp) ->
      Out_channel.with_file (sprintf "%s/%s" dir fn) ~f:(fun ch ->
          Sexp.output_hum ch sexp))

let random_cli =
  let open Command in
  let open Let_syntax in
  let op =
    Arg_type.of_map
    @@ Map.of_alist_exn (module String)
    @@ [
         ("circle", `Circle);
         ("rectangle", `Rect);
         ("union", `Union);
         ("intersection", `Inter);
         ("replicate", `Repl);
       ]
  in
  basic ~summary:"Random benchmarks"
    [%map_open
      let xmax =
        flag "xmax" (optional_with_default 30 int) ~doc:" x dimension size"
      and ymax =
        flag "ymax" (optional_with_default 30 int) ~doc:" y dimension size"
      and seed = flag "seed" (optional_with_default 0 int) ~doc:" random seed"
      and size = flag "size" (required int) ~doc:" program size"
      and ops = flag "op" (listed op) ~doc:" allowed operator"
      and dir = flag "dir" (required string) ~doc:" output directory"
      and n = anon ("n" %: int) in
      fun () ->
        Random.init seed;
        random ~xmax ~ymax ~size ~ops ~n @@ dumps ~dir]

let crosses_cli =
  let open Command.Let_syntax in
  Command.basic ~summary:"Crosses benchmark"
    [%map_open
      let n = anon ("n" %: int) in
      fun () -> crosses n |> Sequence.singleton |> dumps ~dir:"bench/cad2"]

let () =
  Command.group ~summary:"Dump benchmarks"
    [ ("crosses", crosses_cli); ("random", random_cli) ]
  |> Command.run
