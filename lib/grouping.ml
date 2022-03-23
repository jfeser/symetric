open Std

type 'a t = { groups : ('a, 'a list) Hashtbl.t; n_queries : int }

(** return a mapping from representatives to the members of their groups *)
let create_vp m thresh distance states =
  let create_vp = Vpt.create distance `Random in

  let reference_vp = ref (create_vp [])
  and reference = ref []
  and groups = Hashtbl.create m in
  let group_radii = Hashtbl.create m in
  let n_queries = ref 0 in

  let find_close c f =
    incr n_queries;
    if thresh >. 0.0 then (
      if List.length !reference > 100 then (
        reference_vp := create_vp ((Iter.to_list @@ Vpt.iter !reference_vp) @ !reference);
        reference := []);
      (Iter.append
         (Vpt.neighbors distance c thresh !reference_vp)
         (Iter.filter (fun c' -> distance c c' <=. thresh) (Iter.of_list !reference)))
        f)
  in

  states (fun v ->
      let is_empty =
        find_close v
        |> Iter.iter_is_empty (fun c' ->
               Hashtbl.update groups c' ~f:(function None -> [ v ] | Some vs -> v :: vs);
               Hashtbl.update group_radii c' ~f:(function
                 | None -> (distance c' v, 1)
                 | Some (n, d) -> (n +. distance c' v, d + 1)))
      in
      if is_empty then (
        Hashtbl.add_exn group_radii ~key:v ~data:(0.0, 1);
        Hashtbl.add_exn groups ~key:v ~data:[];
        reference := v :: !reference));
  { groups; n_queries = !n_queries }

let%expect_test "" =
  let open Cad_ext in
  let module S = Baseline.Make (Cad_ext) in
  let ctx =
    S.Ctx.create ~max_cost:5
      (Value.Ctx.create (Scene2d.Dim.create ~scaling:2 ~xres:16 ~yres:16 ()))
      (Op.default_operators ~xres:16 ~yres:16)
      (`Pred (fun _ _ -> false))
  in
  let synth = new S.synthesizer ctx in
  synth#run |> ignore;
  let states = S.S.search synth#get_search_state ~cost:5 ~type_:Scene in
  print_s [%message (List.length states : int)]
