open Core
open Staged_synth
open Std

let generate_benchmarks ?(max_states = 100_000) ops ectx cost type_ =
  let module Lang = Tensor in
  let module Synth = Baseline.Make (Lang) in
  let open Synth in
  let config = Ctx.create ~max_cost:cost ~verbose:false ectx ops (`Pred (fun _ _ -> false)) in
  let synth =
    object
      inherit synthesizer config as super

      method! insert_states cost states =
        super#insert_states cost
        @@ List.take
             (List.permute
             @@ List.filter states ~f:(fun (v, _, _) -> match v with Vector x -> List.length x <= 5 | _ -> true))
             max_states
    end
  in
  ignore (synth#run : Lang.Op.t Program.t option);
  let search_state : Search_state.t = synth#get_search_state in
  let attr = Search_state.Attr.create cost type_ in
  Hashtbl.find search_state.values attr |> Option.map ~f:Iter.of_queue |> Option.value ~default:Iter.empty
  |> Iter.map (Search_state.program_exn search_state type_)

let run_abs = false

let run_abs_v2 = true

let run_local = false

let run_local_no_dist_close = false

let run_local_no_dist_far = false

let time_if cond f = if cond then Synth_utils.time f else ""

let () =
  Random.init 0;
  let n_elems = 10 in

  let filter_bench p = Program.count p ~f:(function Tensor.Op.Permute | Reshape -> true | _ -> false) > 0 in

  let input = Tensor.Op.Id { elems = List.init n_elems ~f:Fun.id; shape = [ n_elems ] } in
  let ops =
    let ints = List.init 10 ~f:(fun i -> Tensor.Op.Int (i + 1)) in
    Tensor.Op.[ Permute; Reshape; Cons; Vec; input ] @ ints
  in

  Fmt.pr "cost,abs,local\n%!";

  List.iter [ 12; 14; 16; 18; 20 ] ~f:(fun cost ->
      let benchmarks =
        generate_benchmarks ~max_states:300_000 ops (Tensor.Value.Ctx.create ()) cost Tensor.Type.output
        |> Iter.filter filter_bench |> Iter.take 3 |> Iter.persistent
      in
      eprint_s [%message (benchmarks : Tensor.Op.t Program.t Iter.t)];

      benchmarks
      |> Iter.iter (fun p ->
             let target = Program.eval (Tensor.Value.eval @@ Tensor.Value.Ctx.create ()) p in
             eprint_s [%message "concrete target" (target : Tensor.Value.t)];

             eprint_s [%message (ops : Tensor.Op.t list)];

             let abs = time_if run_abs (fun () -> Abstract_synth_tensor.synth cost target ops) in
             let abs_v2 = time_if run_abs_v2 (fun () -> Abstract_synth_tensor_v2.synth cost target ops) in

             (* for i = 0 to Queue.length abs_refine do *)
             (*   let e = Queue.get abs_refine i and e' = Queue.get abs2_refine i in *)
             (*   print_s *)
             (*     [%message *)
             (*       ((e, e') *)
             (*         : ((Tensor.Op.t * Tensor.Value.t * Abstract_synth_tensor.Abs_value.t) Program.t *)
             (*           * Set.M(Abstract_synth_tensor.Abs_value.Pred).t *)
             (*           * Set.M(Abstract_synth_tensor.Abs_value.Pred).t) *)
             (*           * ((Tensor.Op.t * Tensor.Value.t * Abstract_synth_tensor_v2.Synth.Abs_value.t) Program.t *)
             (*             * Set.M(Abstract_synth_tensor_v2.Synth.Abs_value.Pred).t *)
             (*             * Set.M(Abstract_synth_tensor_v2.Synth.Abs_value.Pred).t))] *)
             (* done; *)
             let local = time_if run_local (fun () -> Local_synth_tensor.synth cost target ops) in
             let local_no_dist_close =
               time_if run_local_no_dist_close (fun () -> Local_synth_tensor.synth ~use_distance:`Close cost target ops)
             in
             let local_no_dist_far =
               time_if run_local_no_dist_far (fun () -> Local_synth_tensor.synth ~use_distance:`Far cost target ops)
             in
             Fmt.pr "%d,%s,%s,%s,%s\n%!" cost abs local local_no_dist_close local_no_dist_far))
