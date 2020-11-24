open Graph_ext

module Make (G : GRAPH) = struct
  let simple graph target =
    Sequence.unfold ~init:(G.succ graph target) ~f:(fun sep ->
        if List.is_empty sep then None
        else
          let sep' =
            List.concat_map sep ~f:(G.succ graph)
            |> List.concat_map ~f:(G.succ graph)
          in
          Some (sep, sep'))
end
