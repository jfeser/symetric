let roots ~is_subset vs =
  List.fold_left vs ~init:[] ~f:(fun roots v ->
      match List.find roots ~f:(fun v' -> is_subset v ~of_:v') with
      | Some _ -> roots
      | None -> v :: List.filter roots ~f:(fun v' -> not (is_subset v' ~of_:v)))
