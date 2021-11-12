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

let circle_repl = Op.(repl 10 10 5 (circle 5 5 5))

let circle = Op.(circle 5 5 5)

let letter_e = Op.(union (rect 5 5 7 25) (repl 0 8 3 @@ rect 7 5 13 9))

let fence = Op.(union (repl 7 0 10 (rect 0 0 2 15)) (repl 0 7 2 (rect 0 4 30 5)))

let checkerboard = Op.(repl 0 8 4 (union (repl 9 0 4 (rect 0 0 3 3)) (repl 9 0 3 (rect 4 4 8 7))))

let benchmarks = [ circle; letter_e; two_circle; three_circle; four_circle ]

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
          |> Iter.iter (fun ((x, y), v) ->
                 if !is_first then is_first := false else Fmt.pf fmt ", ";
                 Fmt.pf fmt "new Example(x = %d, y = %d, v = %d)\n" x y (if v then 1 else 0));
          Fmt.pf fmt "};\n";
          Fmt.pf fmt
            {|
#include "cad.sk"

harness void main(int i) {
  assume (0 <= i && i < NUM_DATA);
  Program solution = gen(DEPTH);
  Vector v = new Vector(x = examples[i].x, y = examples[i].y);
  assert (eval(solution, v, DEPTH) == examples[i].v);
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

  Fmt.pr "cost,abs,local\n%!";

  List.iteri benchmarks ~f:(fun i prog ->
      let target = match Program.eval (Value.eval @@ mk_ectx ()) prog with Scene s -> s | _ -> assert false in
      (* let min_x, min_y, max_x, max_y = *)
      (*   Scene.to_iter size target *)
      (*   |> Iter.fold *)
      (*        (fun ((min_x, min_y, max_x, max_y) as acc) ((x, y), v) -> *)
      (*          if v then (min min_x x, min min_y y, max max_x x, max max_y y) else acc) *)
      (*        (Int.max_value, Int.max_value, Int.min_value, Int.min_value) *)
      (* in *)
      (* let size' = Scene.Size.create ~xres:(max_x - min_x + 1) ~yres:(max_y - min_y + 1) () in *)
      (* let target' = Value.Scene Scene.(crop ~old:size ~new_:size' @@ shift size target (-min_x) (-min_y)) in *)
      let ops = Op.[ Union; Rect; Circle; Repl ] @ List.init (max size.xres size.yres) ~f:(fun i -> Op.Int i) in

      benchmark_to_sketch prog @@ sprintf "cad%d.sk" i;
      let target = Value.Scene target in
      Fmt.epr "%a\n%!" Scene.pp (match target with Scene s -> (size, s) | _ -> assert false);

      let abs = time_if run_abs (fun () -> Abstract_synth_cad.synth size target ops) in
      let local = time_if run_local (fun () -> Local_synth_cad.synth size target ops) in
      let local_no_dist_close = time_if run_local_no_dist_close (fun () -> ()) in
      let local_no_dist_far = time_if run_local_no_dist_far (fun () -> ()) in
      Fmt.pr "%s,%s,%s,%s\n%!" abs local local_no_dist_close local_no_dist_far)
