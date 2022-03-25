open Std

type 'a t = { groups : ('a, 'a list) Hashtbl.t; n_queries : int }

(** return a mapping from representatives to the members of their groups *)
let create_vp m thresh distance states =
  let create_vp = Vpt.create distance `Random in

  let reference_vp = ref (create_vp [])
  and reference = ref []
  and groups = Hashtbl.create m in
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
               Hashtbl.update groups c' ~f:(function None -> [ v ] | Some vs -> v :: vs))
      in
      if is_empty then (
        Hashtbl.add_exn groups ~key:v ~data:[];
        reference := v :: !reference));
  { groups; n_queries = !n_queries }

(** return a mapping from representatives to the members of their groups *)
let create_m (type a) (module M : Base.Hashtbl.Key.S with type t = a) thresh distance
    states =
  let reference = M_tree.empty ~capacity:32 M.compare distance in
  let groups = Hashtbl.create (module M) in
  let n_queries = ref 0 in

  let find_close c f =
    incr n_queries;
    if thresh >. 0.0 then (M_tree.range reference c thresh) f
  in

  states (fun v ->
      let is_empty =
        find_close v
        |> Iter.iter_is_empty (fun c' ->
               Hashtbl.update groups c' ~f:(function None -> [ v ] | Some vs -> v :: vs))
      in
      if is_empty then (
        Hashtbl.add_exn groups ~key:v ~data:[];
        M_tree.insert reference v));
  { groups; n_queries = !n_queries }
