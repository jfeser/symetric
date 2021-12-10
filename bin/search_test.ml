open Core
open Staged_synth
open Cad_ext
module Synth = Local_search_diverse.Make (Cad_ext)
module S = Synth.Search_state
open Std

let size = Scene.Size.create ~xres:13 ~yres:20 ()

let read = false

let () =
  Random.init 0;
  let ectx = Value.Ctx.create size in
  let thresh = 0.15 in
  let distance v v' =
    match (v, v') with
    | Value.Int x, Value.Int x' -> Float.of_int (abs (x - x')) *. 0.3
    | Scene x, Scene x' -> Scene.jaccard x x'
    | _ -> Float.infinity
  in

  let target = Op.(union (union (rect 0 0 3 19) (rect (size.xres - 3) 0 19 19)) (repl 0 8 3 @@ rect 0 0 13 4)) in
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
    Op.[ Union; Circle; Rect; Repl ]
    @ (List.range 0 (max size.xres size.yres) |> List.map ~f:(fun i -> Op.Int i))
    @ (List.range 1 5 |> List.map ~f:(fun i -> Op.Rep_count i))
  in
  let search_state =
    if not read then (
      let filler =
        object
          inherit
            Synth.synthesizer (Synth.Ctx.create ~distance ~search_thresh:(Top_k 0) ~thresh ectx ops target_value) as super

          method generate_states cost =
            let orig = super#generate_states cost in
            let filtered =
              List.filter orig ~f:(fun (v, _, _) ->
                  match v with
                  | Scene s ->
                      Bitarray.hamming_weight (Bitarray.and_ (Scene.pixels target_scene) (Scene.pixels s)) > 0
                      && Bitarray.hamming_weight
                           (Bitarray.and_ (Scene.pixels target_edges) (Scene.pixels @@ Scene.edges size s))
                         > 0
                  | _ -> true)
            in
            print_s [%message (List.length orig : int) (List.length filtered : int)];
            filtered
        end
      in
      for i = 0 to 100 do
        print_s [%message (i : int)];
        filler#fill i
      done;
      let search_state = filler#get_search_state in
      Out_channel.with_file "search_state.sexp" ~f:(fun ch -> Sexp.output ch @@ [%sexp_of: S.t] search_state);
      search_state)
    else In_channel.with_file "search_state.sexp" ~f:(fun ch -> [%of_sexp: S.t] @@ Sexp.input_sexp ch)
  in
  S.print_stats search_state;

  Fmt.pr "Goal:\n%a\n%!" Scene.pp (size, match target_value with Scene s -> s | _ -> assert false);

  Iter.of_hashtbl search_state.values
  |> Iter.map (fun ((key : S.Attr.t), data) ->
         Iter.of_queue data |> Iter.map (fun v -> (distance target_value v, S.TValue.{ value = v; type_ = key.type_ })))
  |> Iter.concat
  |> Iter.top_k ~cmp:(fun (d, _) (d', _) -> [%compare: float] d' d) 30
  |> Iter.iter (fun (d, (tv : S.TValue.t)) ->
         match tv.value with
         | Value.Scene s ->
             Fmt.pr "Searching (d=%f):\n%a\n%!" d Scene.pp (size, s);
             (* S.to_grammar search_state Op.pp S.TValue.{ value = v; type_ = key.type_ }; *)
             let rec repeat n =
               if n > 0 then (
                 S.local_greedy search_state 10 (Value.eval ectx) (distance target_value) tv
                 |> Option.iter ~f:(fun p ->
                        (match Program.eval (Value.eval ectx) p with
                        | Scene s as v ->
                            Fmt.pr "Before local (d=%f):\n%a\n%!" (distance target_value v) Scene.pp (size, s)
                        | _ -> ());
                        let found_value = Program.eval (Value.eval ectx) @@ local p in
                        (match found_value with
                        | Scene s ->
                            Fmt.pr "After local (d=%f):\n%a\n%!" (distance target_value found_value) Scene.pp (size, s)
                        | _ -> ());
                        if [%compare.equal: Value.t] target_value found_value then
                          print_s [%message "success" (p : Op.t Program.t)]
                        (* print_s [%message "failure" (p : Op.t Program.t)] *));
                 repeat (n - 1))
             in
             repeat 10
         | _ -> ())
