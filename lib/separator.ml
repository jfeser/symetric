open Graph_ext

module Make (G : GRAPH) = struct
  let simple graph targets =
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

    let seps = gen_seps [ Set.of_list (module G.V) targets ] in

    Sequence.of_list seps

  let random ?(k = 1) graph targets =
    let next_sep s =
      let no_expand, yes_expand =
        Set.to_list s
        |> List.partition_map ~f:(fun v ->
               match List.concat_map (G.succ graph v) ~f:(G.succ graph) with
               | [] -> First v
               | vs -> Second (v, vs))
      in

      let to_expand, not_expand = List.split_n (List.permute yes_expand) k in

      Set.union_list
        (module G.V)
        [
          Set.of_list (module G.V) no_expand;
          List.concat_map to_expand ~f:Tuple.T2.get2 |> Set.of_list (module G.V);
          List.map not_expand ~f:Tuple.T2.get1 |> Set.of_list (module G.V);
        ]
    in
    let rec gen_seps seps =
      match seps with
      | x :: xs ->
          let x' = next_sep x in
          if Set.equal x x' then seps else gen_seps (x' :: x :: xs)
      | [] -> failwith "no start"
    in

    let seps = gen_seps [ Set.of_list (module G.V) targets ] in

    Sequence.of_list seps
end
