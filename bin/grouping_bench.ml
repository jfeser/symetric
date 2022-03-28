open Core
open Staged_synth
open Grouping

let () =
  let open Cad_ext in
  let module S = Baseline.Make (Cad_ext) in
  let ctx =
    S.Ctx.create ~max_cost:5
      (Value.Ctx.create (Scene2d.Dim.create ~scaling:2 ~xres:16 ~yres:16 ()))
      (Op.default_operators ~xres:16 ~yres:16)
      (`Pred (fun _ _ -> false))
  in
  let synth = new S.synthesizer ctx in
  ignore (synth#run : _);
  let states = S.S.search synth#get_search_state ~cost:5 ~type_:Scene in

  let distance_calls = ref 0 in
  let distance v v' =
    incr distance_calls;
    Value.distance v v'
  in

  let n_groups = 10_000 in
  let groups_vp, vp_time =
    Synth_utils.timed (fun () ->
        create_vp (module Value) 0.2 distance n_groups @@ Iter.of_list states)
  in
  let vp_distance_calls = !distance_calls in
  distance_calls := 0;
  let groups_m, m_time =
    Synth_utils.timed (fun () ->
        create_m (module Value) 0.2 distance n_groups @@ Iter.of_list states)
  in
  let m_distance_calls = !distance_calls in
  print_s
    [%message
      (Hashtbl.length groups_vp.groups : int)
        (Hashtbl.length groups_m.groups : int)
        (vp_time : Time.Span.t)
        (m_time : Time.Span.t)
        (vp_distance_calls : int)
        (m_distance_calls : int)]
