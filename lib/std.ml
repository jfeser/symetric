let ( <. ) = Float.( < )
let ( >. ) = Float.( > )
let ( <=. ) = Float.( <= )
let ( >=. ) = Float.( >= )
let ( =. ) = Float.( = )

module Option = struct
  include Option

  let value_lazy ~default = function Some x -> x | None -> Lazy.force default
end

module Non_empty_list = struct
  type 'a t = 'a * 'a list [@@deriving compare, hash, sexp]

  let[@inline] of_list = function [] -> None | x :: xs -> Some (x, xs)
  let[@inline] of_list_exn = function [] -> failwith "empty list" | x :: xs -> (x, xs)
  let[@inline] to_list (x, xs) = x :: xs
  let[@inline] singleton x = (x, [])
  let[@inline] cons x xs = (x, to_list xs)
  let of_tuple = Fun.id
  let to_tuple = Fun.id
  let[@inline] ( @ ) (x, xs) (y, ys) = (x, xs @ (y :: ys))
  let[@inline] map (x, xs) ~f = (f x, List.map ~f xs)
  let[@inline] hd (x, _) = x
  let[@inline] tl (_, xs) = xs
end

module type HASHABLE = sig
  type t [@@deriving compare, hash, sexp]
end

module Iter = struct
  include Iter

  let sexp_of_t sexp_of_elem iter = [%sexp_of: elem list] @@ Iter.to_list iter
  let t_of_sexp elem_of_sexp sexp = Iter.of_list @@ [%of_sexp: elem list] sexp
  let of_queue q k = Queue.iter q ~f:k
  let of_set s k = Core.Set.iter ~f:k s
  let of_hashtbl x k = Hashtbl.iteri ~f:(fun ~key ~data -> k (key, data)) x
  let of_hashtbl_data x k = Hashtbl.iteri ~f:(fun ~key:_ ~data -> k data) x
  let of_sek_e s k = Sek.E.iter Sek.forward k s
  let iter_while p seq = Iter.take_while p seq (fun _ -> ())

  let to_set (type t w) m iter =
    let module C = (val m : Comparator.S with type t = t and type comparator_witness = w)
    in
    let l = to_list iter |> List.sort ~compare:C.comparator.compare in
    Core.Set.of_list m l

  let list_product iters f =
    let rec product acc = function
      | [] -> f @@ List.rev acc
      | q :: qs -> q (fun x -> product (x :: acc) qs)
    in
    product [] iters

  let%expect_test "" =
    print_s [%message (list_product [ Iter.(1 -- 1) ] : int list t)];
    [%expect {|
      ("list_product [(let open Iter in 1 -- 1)]" ((1))) |}]

  let%expect_test "" =
    print_s
      [%message
        (list_product [ Iter.(1 -- 3); Iter.(1 -- 3); Iter.(1 -- 3) ] : int list t)];
    [%expect
      {|
      ( "list_product\
       \n  [(let open Iter in 1 -- 3);\
       \n  (let open Iter in 1 -- 3);\
       \n  (let open Iter in 1 -- 3)]"
       ((1 1 1) (1 1 2) (1 1 3) (1 2 1) (1 2 2) (1 2 3) (1 3 1) (1 3 2) (1 3 3)
        (2 1 1) (2 1 2) (2 1 3) (2 2 1) (2 2 2) (2 2 3) (2 3 1) (2 3 2) (2 3 3)
        (3 1 1) (3 1 2) (3 1 3) (3 2 1) (3 2 2) (3 2 3) (3 3 1) (3 3 2) (3 3 3))) |}]

  let min_floor ~to_float floor iter =
    Iter.fold_while
      (fun acc v ->
        if [%compare: float] (to_float v) floor <= 0 then (Some v, `Stop)
        else
          let acc' =
            match acc with
            | Some v' when [%compare: float] (to_float v) (to_float v') < 0 -> Some v
            | Some _ -> acc
            | None -> Some v
          in
          (acc', `Continue))
      None iter

  let group_by key_m iter =
    let tbl =
      lazy
        (let tbl = Hashtbl.create key_m in
         iter (fun (k, v) ->
             Hashtbl.update tbl k ~f:(function None -> [ v ] | Some xs -> v :: xs));
         tbl)
    in
    fun k -> Hashtbl.iteri (Lazy.force tbl) ~f:(fun ~key ~data -> k (key, data))

  let mean s =
    let n = ref 0. and d = ref 0 in
    s (fun x ->
        n := !n +. x;
        incr d);
    if Float.(!n > 0.) then Some (!n /. Float.of_int !d) else None

  let stats l =
    let min, max, num, dem =
      Iter.fold
        (fun (min, max, num, dem) x ->
          (Float.min min x, Float.max max x, num +. x, dem +. 1.0))
        (Float.max_value, Float.min_value, 0.0, 0.0)
        l
    in
    (min, max, num /. dem)

  let iter_is_empty f s =
    let is_empty = ref true in
    s (fun x ->
        is_empty := false;
        f x);
    !is_empty

  let last s =
    let last = ref None in
    s (fun x -> last := Some x);
    !last

  let top_k (type t) ~compare k l f =
    let module OM = struct
      type nonrec t = t option

      let compare = Option.compare compare
    end in
    let module H = Binary_heap.Make (OM) in
    assert (k >= 0);
    if k > 0 then (
      let mins = H.create ~dummy:None k in
      l (fun x ->
          if H.length mins < k then H.add mins (Some x)
          else
            let c = compare (Option.value_exn @@ H.minimum mins) x in
            if c < 0 || (c = 0 && Random.bool ()) then (
              ignore (H.pop_minimum mins : t option);
              H.add mins (Some x)));
      H.iter (fun x -> f @@ Option.value_exn x) mins)

  let%expect_test "" =
    print_s [%message (top_k ~compare 3 Iter.(0 -- 10) : int t)];
    [%expect {| ("top_k ~compare 3 (let open Iter in 0 -- 10)" (8 9 10)) |}]

  let top_k_distinct (type k t) (module M : HASHABLE with type t = k) ~score ~key k l f =
    let module OM = struct
      type nonrec t = (float * t) option

      let compare = Option.compare [%compare: float * _]
    end in
    let module H = Binary_heap.Make (OM) in
    assert (k >= 0);
    if k > 0 then (
      let best = H.create ~dummy:None k in
      let contents = Hash_set.create ~size:k ~growth_allowed:false (module M) in
      let add s x =
        H.add best (Some (s, x));
        Hash_set.add contents @@ key x
      in
      l (fun x ->
          if not (Hash_set.mem contents @@ key x) then
            let new_score = score @@ key x in
            if H.length best < k then add new_score x
            else
              let min_score, min_val = Option.value_exn @@ H.minimum best in
              if
                Float.(min_score < new_score || (min_score = new_score && Random.bool ()))
              then (
                ignore (H.pop_minimum best : _ option);
                Hash_set.remove contents @@ key min_val;

                add new_score x));
      H.iter (fun x -> f @@ Tuple.T2.get2 @@ Option.value_exn x) best)

  let%expect_test "" =
    print_s
      [%message
        (top_k_distinct
           (module Int)
           ~score:Float.of_int ~key:Fun.id 3
           (Iter.of_list [ 6; 1; 2; 3; 1; 5; 6 ])
          : int t)];
    [%expect
      {|
      ( "top_k_distinct (module Int) ~score:Float.of_int ~key:Fun.id 3\
       \n  (Iter.of_list [6; 1; 2; 3; 1; 5; 6])" (3 6 5)) |}]

  let top_k_distinct_grouped _ = failwith ""

  let ordered_groupby (type k) (module M : HASHABLE with type t = k) ~score ~key
      ?(batch_size = 10_000) states f =
    let module OM = struct
      type nonrec t = (float * k) option

      let compare = Option.compare [%compare: float * _]
    end in
    let module H = Binary_heap.Make (OM) in
    let groups = Hashtbl.create ~size:batch_size ~growth_allowed:false (module M) in

    let add heap groups s k x =
      H.add heap (Some (s, k));
      Hashtbl.update groups k ~f:(function
        | Some (score, xs) -> (score, x :: xs)
        | None -> (s, [ x ]))
    in

    (* fill the heap with up to batch_size elements *)
    let rec collect_batch max_score =
      let heap = H.create ~dummy:None batch_size in
      Hashtbl.clear groups;

      states (fun x ->
          let k = key x in
          if Hashtbl.mem groups k then
            Hashtbl.update groups k ~f:(function
              | Some (s, xs) -> (s, x :: xs)
              | None -> assert false)
          else
            let new_score = score k in
            if new_score <. max_score then
              if H.length heap < batch_size then add heap groups new_score k x
              else
                let min_score, min_key = Option.value_exn @@ H.minimum heap in
                if
                  Float.(
                    min_score < new_score || (min_score = new_score && Random.bool ()))
                then (
                  ignore (H.pop_minimum heap : _ option);
                  Hashtbl.remove groups min_key;
                  add heap groups new_score k x));

      let groups =
        Hashtbl.to_alist groups
        |> List.sort ~compare:(fun (_, (s, _)) (_, (s', _)) -> [%compare: float] s' s)
      in
      List.iter groups ~f;
      if List.length groups >= batch_size then
        let _, (min_score, _) = List.last_exn groups in
        collect_batch min_score
    in

    collect_batch Float.infinity

  let%expect_test "" =
    print_s
      [%message
        (ordered_groupby
           (module Int)
           ~score:Float.of_int ~key:Fun.id ~batch_size:1
           (Iter.of_list [ 6; 1; 2; 3; 1; 5; 6 ])
          : (int * (float * int list)) t)];
    [%expect
      {|
      ( "ordered_groupby (module Int) ~score:Float.of_int ~key:Fun.id ~batch_size:1\
       \n  (Iter.of_list [6; 1; 2; 3; 1; 5; 6])"
       ((6 (6 (6 6))) (5 (5 (5))) (3 (3 (3))) (2 (2 (2))) (1 (1 (1 1))))) |}]
end

module List = struct
  include List

  let product l = List.reduce_exn l ~f:( * )

  let group_by m l =
    Hashtbl.of_alist_multi m l |> Iter.of_hashtbl
    |> Iter.map (fun (k, v) -> (k, Non_empty_list.of_list_exn v))
    |> Iter.to_list

  let take ~n l = List.take l n

  let set l i x =
    assert (i >= 0 && i < List.length l);
    List.take l i @ (x :: List.drop l (i + 1))

  let%test_unit "" =
    [%test_result: int list] ~expect:[ 4; 2; 3 ] (set [ 1; 2; 3 ] 0 4);
    [%test_result: int list] ~expect:[ 1; 4; 3 ] (set [ 1; 2; 3 ] 1 4);
    [%test_result: int list] ~expect:[ 1; 2; 4 ] (set [ 1; 2; 3 ] 2 4)

  let rank ~compare l =
    List.mapi l ~f:(fun i x -> (x, i))
    |> List.sort ~compare:(fun (x, _) (x', _) -> compare x x')
    |> List.mapi ~f:(fun r (_, i) -> (i, r))
    |> List.sort ~compare:(fun (i, _) (i', _) -> [%compare: int] i i')
    |> List.map ~f:(fun (_, r) -> r)

  let%test_unit "rank" =
    [%test_result: int list] ~expect:[ 2; 0; 1 ]
    @@ rank ~compare:[%compare: int] [ 3; 1; 2 ]

  let rec insert l i v =
    if i = 0 then v :: l
    else
      match l with
      | [] -> failwith "index out of bounds"
      | x :: xs -> x :: insert xs (i - 1) v

  let%test_unit "insert" =
    [%test_result: int list] ~expect:[ 9; 1; 2; 3 ] (insert [ 1; 2; 3 ] 0 9);
    [%test_result: int list] ~expect:[ 1; 9; 2; 3 ] (insert [ 1; 2; 3 ] 1 9);
    [%test_result: int list] ~expect:[ 1; 2; 9; 3 ] (insert [ 1; 2; 3 ] 2 9);
    [%test_result: int list] ~expect:[ 1; 2; 3; 9 ] (insert [ 1; 2; 3 ] 3 9)
end

module Array = struct
  let mean a =
    let n = Array.sum (module Float) a ~f:Fun.id and d = Float.of_int @@ Array.length a in
    n /. d

  let stddev a =
    let mean = mean a in
    let d = Array.fold a ~init:0.0 ~f:(fun s x -> s +. ((x -. mean) *. (x -. mean))) in
    let d = d /. (Float.of_int @@ Array.length a) in
    Float.sqrt d

  include Array

  let median compare a =
    let a' = Array.sorted_copy a ~compare in
    a'.(Array.length a' / 2)
end

module Incr_mean = struct
  type t = float * float [@@deriving sexp]

  let zero = (0., 0.)
  let add (n, d) x = (n +. x, d +. 1.)
  let mean (n, d) = n /. d
end

let rank_stability n ranks =
  if n <= 1 then Error `Not_enough_objects
  else
    let totals, m =
      Iter.fold
        (fun (totals, m) rank ->
          let rank = List.map ~f:Float.of_int rank in
          let totals' = List.map2_exn totals rank ~f:( +. ) in
          let m' = m +. 1. in
          (totals', m'))
        (List.init n ~f:(fun _ -> 0.), 0.)
        ranks
    in
    if m <=. 0. then Error `No_ranks
    else
      let mean = Option.value_exn @@ Iter.mean @@ Iter.of_list totals in
      let sum_sqd_dev =
        Iter.sumf
        @@ Iter.map (fun tot -> (tot -. mean) *. (tot -. mean))
        @@ Iter.of_list totals
      in
      let n = Float.of_int n in
      Ok (12. *. sum_sqd_dev /. (m *. m *. ((n *. n *. n) -. n)))

let%expect_test "rank-stability" =
  print_s
    [%message
      (rank_stability 3 @@ Iter.of_list [ [ 1; 2; 3 ]; [ 1; 2; 3 ]; [ 1; 2; 3 ] ]
        : (float, _) Result.t)];
  print_s
    [%message
      (rank_stability 3 @@ Iter.of_list [ [ 1; 2; 3 ]; [ 2; 1; 3 ]; [ 1; 2; 3 ] ]
        : (float, _) Result.t)];
  print_s
    [%message
      (rank_stability 3 @@ Iter.of_list [ [ 1; 2; 3 ]; [ 2; 3; 1 ]; [ 3; 1; 2 ] ]
        : (float, _) Result.t)];
  [%expect
    {|
    ("(rank_stability 3) @@ (Iter.of_list [[1; 2; 3]; [1; 2; 3]; [1; 2; 3]])"
     (Ok 1))
    ("(rank_stability 3) @@ (Iter.of_list [[1; 2; 3]; [2; 1; 3]; [1; 2; 3]])"
     (Ok 0.77777777777777779))
    ("(rank_stability 3) @@ (Iter.of_list [[1; 2; 3]; [2; 3; 1]; [3; 1; 2]])"
     (Ok 0)) |}]
