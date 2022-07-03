open Std

type 'a t = {
  groups : ('a, 'a list) Hashtbl.t;
  n_queries : int;
  n_samples : int;
  runtime : Time.Span.t;
}

(** return a mapping from representatives to the members of their groups *)
let create_vp m thresh distance k states =
  let create_vp = Vpt.create distance `Random in

  let reference_vp = ref (create_vp [])
  and reference = ref []
  and groups = Hashtbl.create m in
  let n_queries = ref 0 and n_samples = ref 0 in

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

  With_return.with_return (fun r ->
      states (fun v ->
          if Hashtbl.mem groups v then Hashtbl.add_multi groups ~key:v ~data:v
          else (
            incr n_samples;
            let no_existing_group =
              find_close v
              |> Iter.iter_is_empty (fun c' -> Hashtbl.add_multi groups ~key:c' ~data:c')
            in
            if no_existing_group then (
              if Hashtbl.length groups = k then
                r.return
                  {
                    groups;
                    n_queries = !n_queries;
                    n_samples = !n_samples;
                    runtime = Time.Span.zero;
                  };
              Hashtbl.add_exn groups ~key:v ~data:[ v ];
              reference := v :: !reference)));
      r.return
        {
          groups;
          n_queries = !n_queries;
          n_samples = !n_samples;
          runtime = Time.Span.zero;
        })

(** return a mapping from representatives to the members of their groups *)
let create_m (type a) (module M : Base.Hashtbl.Key.S with type t = a) thresh distance k
    states =
  let reference = M_tree.empty ~capacity:32 M.compare distance in
  let groups = Hashtbl.create (module M) in
  let n_queries = ref 0 and n_samples = ref 0 in

  let find_close c f =
    incr n_queries;
    if thresh >. 0.0 then (M_tree.range reference c thresh) f
  in

  let runtime = ref Time.Span.zero in
  with_return (fun r ->
      states (fun v ->
          Synth_utils.timed (`Add runtime) (fun () ->
              if Hashtbl.mem groups v then Hashtbl.add_multi groups ~key:v ~data:v
              else (
                incr n_samples;
                let no_existing_group =
                  find_close v
                  |> Iter.iter_is_empty (fun c' ->
                         Hashtbl.add_multi groups ~key:c' ~data:c')
                in
                if no_existing_group then (
                  if Hashtbl.length groups = k then
                    r.return
                      {
                        groups;
                        n_queries = !n_queries;
                        n_samples = !n_samples;
                        runtime = !runtime;
                      };
                  Hashtbl.add_exn groups ~key:v ~data:[ v ];
                  M_tree.insert reference v))));
      r.return
        { groups; n_queries = !n_queries; n_samples = !n_samples; runtime = !runtime })
