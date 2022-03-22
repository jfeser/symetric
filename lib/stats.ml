type t = float ref Hashtbl.M(String).t

let create () = Hashtbl.create (module String)

let add_probe_exn ?(init = Float.nan) stats name =
  let data = ref init in
  Hashtbl.add_exn stats ~key:name ~data;
  data

let to_json stats =
  `Assoc (Hashtbl.to_alist stats |> List.map ~f:(fun (k, v) -> (k, `Float !v)))
