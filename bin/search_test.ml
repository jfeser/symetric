open Core
open Staged_synth
open Cad_ext
module Synth = Local_search_diverse.Make (Cad_ext)
module S = Synth.Search_state
open Std

let size = Scene.Size.create ~xres:12 ~yres:20 ()
let read = false

let run ~max_cost ~thresh target =
  Random.self_init ();

  let ectx = Value.Ctx.create size in
  let distance v v' =
    match (v, v') with
    | Value.Int x, Value.Int x' -> Float.of_int (abs (x - x')) *. 0.3
    | Scene x, Scene x' -> Scene.jaccard x x'
    | _ -> Float.infinity
  in

  let target_value = Program.eval (Value.eval ectx) target in
  let target_scene = match target_value with Scene s -> s | _ -> assert false in
  let target_edges = Scene.edges size target_scene in
  Fmt.pr "Goal:\n%a\n%!" Scene.pp (size, target_edges);

  let local p =
    Local_search.of_unnormalize_tabu ~target:target_value ~dist:distance
      (module Op)
      (module Value)
      (function
        | Apply (Int x, []) ->
            if x = 0 then [ Apply (Int (x + 1), []) ]
            else if x = max size.xres size.yres then [ Apply (Int (x - 1), []) ]
            else [ Apply (Int (x + 1), []); Apply (Int (x - 1), []) ]
        | _ -> [])
      (Program.eval (Value.eval ectx))
      p
    |> Iter.map (fun p -> (distance target_value (Program.eval (Value.eval ectx) p), p))
    |> Iter.take 1000
    |> Iter.min ~lt:(fun (d, _) (d', _) -> Float.(d < d'))
    |> Option.map ~f:(fun (_, p) -> p)
    |> Option.value ~default:p
  in

  let ops =
    Op.[ Union; Circle; Rect; Repl; Sub ]
    @ (List.range 0 (max size.xres size.yres) |> List.map ~f:(fun i -> Op.Int i))
    @ (List.range 2 5 |> List.map ~f:(fun i -> Op.Rep_count i))
  in
  let search_state =
    let filler =
      object
        inherit
          Synth.synthesizer
            (Synth.Ctx.create ~distance ~search_thresh:(Top_k 0) ~thresh ectx ops
               target_value) as super

        method! generate_states cost =
          let orig = super#generate_states cost in
          let filtered =
            List.filter orig ~f:(fun (v, _, _) ->
                match v with
                | Scene s ->
                    Bitarray.hamming_weight
                      (Bitarray.and_ (Scene.pixels target_scene) (Scene.pixels s))
                    > 0
                    && Bitarray.hamming_weight
                         (Bitarray.and_ (Scene.pixels target_edges)
                            (Scene.pixels @@ Scene.edges size s))
                       > 0
                | _ -> true)
          in
          print_s [%message (List.length orig : int) (List.length filtered : int)];
          filtered
      end
    in
    for i = 0 to max_cost do
      print_s [%message (i : int)];
      filler#fill i
    done;
    filler#get_search_state
  in
  S.print_stats search_state;

  Fmt.pr "Goal:\n%a\n%!" Scene.pp
    (size, match target_value with Scene s -> s | _ -> assert false);

  Iter.of_hashtbl search_state.values
  |> Iter.map (fun ((key : S.Attr.t), data) ->
         Iter.of_queue data
         |> Iter.map (fun v ->
                (distance target_value v, S.TValue.{ value = v; type_ = key.type_ })))
  |> Iter.concat
  |> Iter.top_k ~cmp:(fun (d, _) (d', _) -> [%compare: float] d' d) 100
  |> Iter.sort ~cmp:(fun (d, _) (d', _) -> [%compare: float] d d')
  |> Iter.iter (fun (d, (tv : S.TValue.t)) ->
         match tv.value with
         | Value.Scene s ->
             Fmt.pr "Searching (d=%f):\n%a\n%!" d Scene.pp (size, s);
             let rec repeat n =
               if n > 0 then (
                 (* let p_basic = S.program_of_class_exn search_state tv in *)
                 (* let p_basic_local = local p_basic in *)
                 (* print_s [%message (p_basic_local : Op.t Program.t)]; *)
                 (* (match Program.eval (Value.eval ectx) @@ local p_basic_local with *)
                 (* | Scene s as v -> Fmt.pr "After local (d=%f):\n%a\n%!" (distance target_value v) Scene.pp (size, s) *)
                 (* | _ -> ()); *)
                 let p =
                   S.local_greedy (Value.pp ectx) search_state (Int.ceil_log2 max_cost)
                     (Value.eval ectx) (distance target_value) tv
                   |> Option.value_exn
                 in
                 (* (match Program.eval (Value.eval ectx) p with *)
                 (* | Scene s as v -> Fmt.pr "Before local (d=%f):\n%a\n%!" (distance target_value v) Scene.pp (size, s) *)
                 (* | _ -> ()); *)
                 let found_value = Program.eval (Value.eval ectx) @@ local p in
                 (* (match found_value with *)
                 (* | Scene s -> Fmt.pr "After local (d=%f):\n%a\n%!" (distance target_value found_value) Scene.pp (size, s) *)
                 (* | _ -> ()); *)
                 if [%compare.equal: Value.t] target_value found_value then
                   raise_s [%message "success" (p : Op.t Program.t)]
                   (* print_s [%message "failure" (p : Op.t Program.t)] *);
                 repeat (n - 1))
             in
             repeat 10
         | _ -> ())

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate a synthesizer."
    [%map_open
      let max_cost =
        flag "-cost"
          (optional_with_default 22 int)
          ~doc:" the maximum size of program to evaluate"
      and thresh =
        flag "-thresh"
          (optional_with_default 0.2 float)
          ~doc:"distance threshold to trigger grouping"
      in
      fun () -> run ~max_cost ~thresh @@ Cad_ext.parse @@ Sexp.input_sexp In_channel.stdin]
  |> Command.run
