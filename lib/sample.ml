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
