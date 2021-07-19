let weighted_random ?(state = Random.State.default) ~weight n elems =
  let module H = Pairing_heap in
  let compare (k, _) (k', _) = -[%compare: float] k k' in
  let heap = H.create ~cmp:compare () in
  List.iter elems ~f:(fun v ->
      let k =
        let r = Random.State.float_range state (Float.one_ulp `Up 0.0) 1.0 in
        -.(Float.log r /. weight v)
      in
      if H.length heap < n then H.add heap (k, v)
      else match H.pop_if heap (fun (k_max, _) -> Float.O.(k < k_max)) with Some _ -> H.add heap (k, v) | None -> ());
  Pairing_heap.to_list heap |> List.map ~f:Tuple.T2.get2

let%expect_test "" =
  for _ = 0 to 10 do
    weighted_random 5 [ 100.0; 10.0; 10.0; 10.0; 10.0; 10.0; 1.0; 1.0; 1.0; 1.0; 1.0 ] ~weight:Fun.id
    |> [%sexp_of: float list] |> print_s
  done;
  [%expect
    {|
    (10 10 10 100 10)
    (10 10 10 10 100)
    (10 10 10 10 100)
    (10 10 100 10 10)
    (10 10 100 10 10)
    (1 10 10 10 100)
    (1 10 10 100 10)
    (1 1 10 100 10)
    (10 10 10 100 10)
    (1 10 10 10 100)
    (10 10 10 10 100) |}]

module Incremental = struct
  type ('a, 'b) t = { add : 'a -> 'b; get_sample : unit -> 'a list }

  let weighted_reservoir ?(state = Random.State.default) n =
    let module H = Pairing_heap in
    let compare (k, _) (k', _) = -[%compare: float] k k' in
    let heap = H.create ~cmp:compare () in

    let add v weight =
      let k =
        let r = Random.State.float_range state (Float.one_ulp `Up 0.0) 1.0 in
        -.(Float.log r /. weight)
      in
      if H.length heap < n then H.add heap (k, v)
      else match H.pop_if heap (fun (k_max, _) -> Float.O.(k < k_max)) with Some _ -> H.add heap (k, v) | None -> ()
    in
    let get_sample () = Pairing_heap.to_list heap |> List.map ~f:Tuple.T2.get2 in
    { add; get_sample }

  let reservoir ?(state = Random.State.default) n =
    let wr = weighted_reservoir ~state n in
    let add v = wr.add v 1.0 and get_sample = wr.get_sample in
    { add; get_sample }

  let reservoir_unique ?(state = Random.State.default) cmp n =
    let sample = ref (Set.empty cmp) and idx = ref 0 in
    let add v =
      (if !idx < n then sample := Set.add !sample v
      else
        let j = Random.State.int_incl state 1 !idx in
        if j <= n then sample := Set.add (Set.remove_index !sample j) v);
      incr idx
    and get_sample () = Set.to_list !sample in
    { add; get_sample }
end
