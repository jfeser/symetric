open Graph_ext

module Make (G : GRAPH) = struct
  let simple graph target =
    let next_sep s =
      Set.to_list s
      |> List.concat_map ~f:(fun v ->
             match List.concat_map (G.succ graph v) ~f:(G.succ graph) with
             | [] -> [ v ]
             | vs -> vs)
      |> Set.of_list (module G.V)
    in
    let rec gen_seps seps =
      match seps with
      | x :: xs ->
          let x' = next_sep x in
          if Set.equal x x' then seps else gen_seps (x' :: x :: xs)
      | [] -> failwith "no start"
    in

    let seps = gen_seps [ Set.singleton (module G.V) target ] in

    Sequence.of_list seps
end
