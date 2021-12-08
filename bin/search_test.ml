open Core
open Staged_synth
module Search_state = Search_state_all.Make (Cad_ext)
open Search_state
open Cad_ext

let size = Scene.Size.create ~xres:13 ~yres:20 ()

let () =
  let ectx = Value.Ctx.create size in
  let distance = Local_synth_cad.jaccard in
  let target = Op.(union (circle 3 12 4) (circle 17 11 6)) in
  let target_value = Program.eval (Value.eval ectx) target in
  let search_state = [%of_sexp: t] @@ Sexp.input_sexp In_channel.stdin in

  print_stats search_state;

  Fmt.pr "Goal:\n%a\n%!" Scene.pp (size, match target_value with Scene s -> s | _ -> assert false);

  Hashtbl.iteri search_state.values ~f:(fun ~key ~data:q ->
      if key.cost = 9 then
        Queue.iter
          ~f:(function
            | Value.Scene s as v ->
                Fmt.pr "Searching:\n%a\n%!" Scene.pp (size, s);
                Search_state.local_search2 search_state 9 (Value.eval ectx) (distance target_value)
                  { type_ = key.type_; value = v }
                |> Gen.iter (fun (d, p) ->
                       if [%compare.equal: Value.t] target_value (Program.eval (Value.eval ectx) p) then
                         raise_s [%message "found program" (d : float) (p : Op.t Program.t)])
            | _ -> ())
          q)
