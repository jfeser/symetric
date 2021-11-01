open Core
open Staged_synth
open Std
module Lang = Cad_ext
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

let two_circle = Op.(union (circle 10 15 5) (circle 15 15 5))

let benchmarks = [ two_circle ]

let run_abs = true

let run_local = true

let run_local_no_dist_close = true

let run_local_no_dist_far = true

let time_if cond f = if cond then Synth_utils.time f else ""

let () =
  Random.init 0;

  let ops = Op.[ Union; Circle ] @ List.init 30 ~f:(fun i -> Op.Int i) in

  let size = Scene.Size.create ~xres:30 ~yres:30 () in
  let mk_ectx () = Value.Ctx.create size in

  Fmt.pr "cost,abs,local\n%!";

  List.iter benchmarks ~f:(fun prog ->
      let target = Program.eval (Value.eval @@ mk_ectx ()) prog in
      Fmt.epr "%a\n%!" Scene.pp (match target with Scene s -> (size, s) | _ -> assert false);

      let abs = time_if run_abs (fun () -> Abstract_synth_cad.synth size target ops) in
      let local = time_if run_local (fun () -> Local_synth_cad.synth size target ops) in
      let local_no_dist_close = time_if run_local_no_dist_close (fun () -> ()) in
      let local_no_dist_far = time_if run_local_no_dist_far (fun () -> ()) in
      Fmt.pr "%s,%s,%s,%s\n%!" abs local local_no_dist_close local_no_dist_far)
