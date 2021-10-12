open Core
open Staged_synth
open Std
module Lang = Cad
open Lang

let generate_benchmarks ?(max_states = 1_000) ops ectx cost type_ =
  let module Synth = Baseline.Make (Lang) in
  let open Synth in
  let config = Ctx.create ~max_cost:cost ~verbose:true ectx ops (`Pred (fun _ _ -> false)) in
  let synth =
    object
      inherit synthesizer config as super

      method! insert_states cost states = super#insert_states cost @@ List.take (List.permute states) max_states
    end
  in
  ignore (synth#run : Lang.Op.t Program.t option);
  let search_state : Search_state.t = synth#get_search_state in
  let attr = Search_state.Attr.create cost type_ in
  Hashtbl.find search_state.values attr |> Option.map ~f:Iter.of_queue |> Option.value ~default:Iter.empty
  |> Iter.map (Search_state.program_exn search_state type_)

let run_abs = true

let run_local = true

let run_local_no_dist_close = true

let run_local_no_dist_far = true

let time_if cond f = if cond then Synth_utils.time f else Time.Span.zero

let () =
  Random.init 0;

  let ops = Op.[ inter; union ] @ Cad_utils.grid 30 30 16 (Op.circle ~id:0 ~center:Vector2.zero ~radius:3.0) in

  let mk_ectx () = Value.Ctx.create ~xlen:30 ~ylen:30 () in

  Fmt.pr "cost,abs,local\n%!";

  List.iter [ 11 ] ~f:(fun cost ->
      let benchmarks = generate_benchmarks ops (mk_ectx ()) cost Type.output |> Iter.take 3 |> Iter.persistent in
      eprint_s [%message (benchmarks : Op.t Program.t Iter.t)];

      benchmarks
      |> Iter.map (fun p ->
             let target = Program.eval (Value.eval @@ mk_ectx ()) p in
             eprint_s [%message "concrete target" (target : Value.t)];

             let abs = time_if run_abs (fun () -> ()) in
             let local = time_if run_local (fun () -> ()) in
             let local_no_dist_close = time_if run_local_no_dist_close (fun () -> ()) in
             let local_no_dist_far = time_if run_local_no_dist_far (fun () -> ()) in
             (abs, local, local_no_dist_close, local_no_dist_far))
      |> Iter.iter (fun (abs, local, local_no_dist_close, local_no_dist_far) ->
             let time_pp fmt ts = Fmt.pf fmt "%f" @@ Time.Span.to_ms ts in
             Fmt.pr "%d,%a,%a,%a,%a\n%!" cost time_pp abs time_pp local time_pp local_no_dist_close time_pp
               local_no_dist_far))
