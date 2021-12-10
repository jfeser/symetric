open Core
open Staged_synth
open Cad_ext
module Synth = Local_search_diverse.Make (Cad_ext)
module S = Synth.Search_state

let size = Scene.Size.create ~xres:13 ~yres:20 ()

let read = true

let () =
  let ectx = Value.Ctx.create size in
  let distance = Local_synth_cad.jaccard in
  let target = Op.(circle 3 12 4) in
  let target_value = Program.eval (Value.eval ectx) target in
  let ops =
    Op.[ Union; Circle; Rect; Repl ] @ (List.range 0 (max size.xres size.yres) |> List.map ~f:(fun i -> Op.Int i))
  in
  let search_state =
    if not read then (
      let filler =
        new Synth.synthesizer (Synth.Ctx.create ~distance ~search_thresh:(Top_k 0) ~thresh:0.1 ectx ops target_value)
      in
      for i = 0 to 5 do
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

  Hashtbl.iteri search_state.values ~f:(fun ~key ~data:q ->
      if key.cost = 4 then
        Queue.iter
          ~f:(fun v ->
            match v with
            | Value.Scene s as v ->
                Fmt.pr "Searching:\n%a\n%!" Scene.pp (size, s);
                S.to_grammar search_state Op.pp S.TValue.{ value = v; type_ = key.type_ };
                let rec repeat n =
                  if n > 0 then (
                    S.local_greedy search_state 9 (Value.eval ectx) (distance target_value)
                      S.{ type_ = key.type_; value = v }
                    |> Option.iter ~f:(fun p ->
                           let found_value = Program.eval (Value.eval ectx) p in
                           (* (match found_value with Scene s -> Fmt.pr "Found:\n%a\n%!" Scene.pp (size, s) | _ -> ()); *)
                           if [%compare.equal: Value.t] target_value found_value then
                             print_s [%message "success" (p : Op.t Program.t)]
                           (* print_s [%message "failure" (p : Op.t Program.t)] *));
                    repeat (n - 1))
                in
                repeat 10
            | _ -> ())
          q)
