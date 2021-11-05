open Core
open Staged_synth
open Std
module Lang = Cad_ext
open Lang

let size = Scene.Size.create ~xres:30 ~yres:30 ()

let mk_ectx () = Value.Ctx.create size

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

let two_circle = Op.(union (circle 5 15 5) (circle 15 15 5))

let three_circle = Op.(union (circle 5 15 5) @@ union (circle 15 15 5) (circle 25 15 5))

let four_circle = Op.(union (circle 5 15 5) @@ union (circle 15 15 5) @@ union (circle 25 15 5) (circle 20 5 5))

let benchmarks = [ two_circle; three_circle; four_circle ]

let benchmark_to_sketch p fn =
  let target = Program.eval (Value.eval @@ mk_ectx ()) p in
  match target with
  | Scene s ->
      let is_first = ref true in
      Out_channel.with_file fn ~f:(fun ch ->
          let fmt = Format.formatter_of_out_channel ch in
          Fmt.pf fmt "int NUM_DATA = %d;\n" (Scene.npixels s);
          Fmt.pf fmt "Example[NUM_DATA] examples = {\n";
          Scene.to_iter size s
          |> Iter.iter (fun ((x, y, _, _), v) ->
                 if !is_first then is_first := false else Fmt.pf fmt ", ";
                 Fmt.pf fmt "new Example(x = %d, y = %d, v = %d)\n" x y (if v then 1 else 0));
          Fmt.pf fmt "};\n";
          Fmt.pf fmt
            {|
#include "cad.sk"

harness void main(int i) {
  assume (0 <= i && i < NUM_DATA);
  Vector p = new Vector(x = examples[i].x, y = examples[i].y);
  assert (contains(p) == examples[i].v);
}                    
                    |})
  | _ -> assert false

let run_abs = false

let run_local = true

let run_local_no_dist_close = false

let run_local_no_dist_far = false

let time_if cond f = if cond then Synth_utils.time f else ""

let () =
  Random.init 0;

  let ops = Op.[ Union; Rect; Circle ] @ List.init 30 ~f:(fun i -> Op.Int i) in

  Fmt.pr "cost,abs,local\n%!";

  List.iteri benchmarks ~f:(fun i prog ->
      let target = Program.eval (Value.eval @@ mk_ectx ()) prog in
      benchmark_to_sketch prog @@ sprintf "cad%d.sk" i;
      Fmt.epr "%a\n%!" Scene.pp (match target with Scene s -> (size, s) | _ -> assert false);

      let abs = time_if run_abs (fun () -> Abstract_synth_cad.synth size target ops) in
      let local = time_if run_local (fun () -> Local_synth_cad.synth size target ops) in
      let local_no_dist_close = time_if run_local_no_dist_close (fun () -> ()) in
      let local_no_dist_far = time_if run_local_no_dist_far (fun () -> ()) in
      Fmt.pr "%s,%s,%s,%s\n%!" abs local local_no_dist_close local_no_dist_far)
