open Std

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
      else
        match H.pop_if heap (fun (k_max, _) -> k <. k_max) with
        | Some _ -> H.add heap (k, v)
        | None -> ());
  Pairing_heap.to_list heap |> List.map ~f:Tuple.T2.get2

let%expect_test "" =
  for _ = 0 to 10 do
    weighted_random 5
      [ 100.0; 10.0; 10.0; 10.0; 10.0; 10.0; 1.0; 1.0; 1.0; 1.0; 1.0 ]
      ~weight:Fun.id
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
  type ('e, 's, 'b) t = { add : 'e -> 'b; get_sample : unit -> 's }

  let weighted ?(state = Random.State.default) () =
    let heap = ref None in
    let add v weight =
      let k =
        let r = Random.State.float_range state (Float.one_ulp `Up 0.0) 1.0 in
        -.(Float.log r /. weight)
      in
      match !heap with
      | None -> heap := Some (k, v)
      | Some (k_max, _) when k <. k_max -> heap := Some (k, v)
      | Some _ -> ()
    in
    let get_sample () = !heap in
    { add; get_sample }

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
      else
        match H.pop_if heap (fun (k_max, _) -> k <. k_max) with
        | Some _ -> H.add heap (k, v)
        | None -> ()
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

let stochastic ?(n = 5) ~score ~propose t f =
  let rec loop i t v =
    if i < n then (
      f (t, v);

      let t' = propose t in
      let v' = score t' in
      let ratio = v /. v' in
      let accept = Random.float 1.0 in
      if accept <. ratio then loop (i + 1) t' v' else loop (i + 1) t v)
  in
  loop 0 t (score t)

module Quantile_estimator = struct
  type state = Filling | Skipping of (float * int)

  type 'a t = {
    sample : 'a array;
    compare : 'a -> 'a -> int;
    quantiles : float list;
    mutable state : state;
    mutable i : int;
  }

  let create ?(epsilon = 0.1) ?(delta = 0.05) ?(quantiles = [ 0.; 0.25; 0.5; 0.75; 1. ])
      compare default =
    assert (List.for_all quantiles ~f:(fun q -> q >=. 0. && q <=. 1.));

    (* see https://sites.cs.ucsb.edu/~suri/psdir/ency.pdf *)
    let sample_n = Float.(to_int (log (1. /. delta) /. (epsilon *. epsilon))) in
    {
      sample = Array.create ~len:sample_n default;
      i = 0;
      state = Filling;
      quantiles;
      compare;
    }

  let[@inline] get_w n_sample = Float.(exp (log (Random.float 1.0) /. of_int n_sample))
  let[@inline] get_next i w = i + Float.(to_int (log (Random.float 1.0) /. log (1. -. w)))

  let add this x =
    let n_sample = Array.length this.sample in
    match this.state with
    | Filling ->
        assert (this.i < n_sample);
        this.sample.(this.i) <- x;
        this.i <- this.i + 1;

        if this.i >= n_sample then
          let w = get_w n_sample in
          let next = get_next this.i w in
          this.state <- Skipping (w, next)
    | Skipping (w, next) ->
        if this.i < next then this.i <- this.i + 1
        else (
          this.sample.(Random.int n_sample) <- x;
          let w = w *. get_w n_sample in
          let next = get_next this.i w in
          this.state <- Skipping (w, next))

  let quantiles this =
    let sample_n =
      match this.state with
      | Filling -> Int.max 0 (this.i - 1)
      | Skipping _ -> Array.length this.sample - 1
    in
    Array.sort this.sample ~compare:this.compare;
    List.map this.quantiles ~f:(fun q ->
        (q, this.sample.(Float.(to_int (q *. of_int sample_n)))))

  let yojson_of_t yojson_of_a this = [%yojson_of: (float * a) list] @@ quantiles this

  let%expect_test "" =
    let est = create [%compare: int] 0 in
    print_s [%message (quantiles est : (float * int) list)];
    [%expect {| ("quantiles est" ((0 0) (0.25 0) (0.5 0) (0.75 0) (1 0))) |}]

  let%expect_test "" =
    let est = create [%compare: int] 0 in
    Iter.of_list [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]
    |> Iter.cycle |> Iter.take 1000
    |> Iter.iter (add est);

    match quantiles est with
    | [ (0., min); (0.25, lq); (0.5, med); (0.75, uq); (1., max) ] ->
        [%test_pred: int] ~message:"min" (fun min -> min = 1) min;
        [%test_pred: int] ~message:"lower quartile" (fun lq -> lq = 3) lq;
        [%test_pred: int] ~message:"median" (fun med -> med = 5) med;
        [%test_pred: int] ~message:"upper quartile" (fun uq -> uq = 8) uq;
        [%test_pred: int] ~message:"max" (fun max -> max = 10) max
    | qs -> raise_s [%message "unexpected quantiles" (qs : (float * int) list)]
end
