open Graph_ext

module Make (G : GRAPH) = struct
  let simple graph target =
    Sequence.unfold
      ~init:(G.succ graph target |> Set.of_list (module G.V))
      ~f:(fun sep ->
        if Set.is_empty sep then None
        else
          let sep' =
            Set.to_sequence sep
            |> Sequence.map ~f:(fun v ->
                   G.succ graph v |> List.concat_map ~f:(G.succ graph))
            |> Sequence.to_list |> List.concat
            |> Set.of_list (module G.V)
          in
          Some (sep, sep'))
    |> Sequence.to_list |> List.rev |> Sequence.of_list
end
