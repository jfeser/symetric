open Graph_ext

module Make (G : GRAPH) = struct
  let simple graph target =
    let next_sep s =
      Set.to_list s
      |> List.concat_map ~f:(fun v ->
             match G.succ graph v with
             | [] -> [ v ]
             | vs -> List.concat_map vs ~f:(G.succ graph))
      |> Set.of_list (module G.V)
    in
    let rec gen_seps seps =
      match seps with
      | x :: xs ->
          let x' = next_sep x in
          if Set.equal x x' then seps else gen_seps (x' :: x :: xs)
      | [] -> failwith "no start"
    in

    let seps = gen_seps [ G.succ graph target |> Set.of_list (module G.V) ] in
    Sequence.of_list seps
end
