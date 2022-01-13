module Option = struct
  include Option

  let value_lazy ~default = function Some x -> x | None -> Lazy.force default
end

module List = struct
  include List

  let product l = List.reduce_exn l ~f:( * )
  let group_by m l = Hashtbl.of_alist_multi m l |> Hashtbl.to_alist
end

module Iter = struct
  include Iter

  let sexp_of_t sexp_of_elem iter = [%sexp_of: elem list] @@ Iter.to_list iter
  let t_of_sexp elem_of_sexp sexp = Iter.of_list @@ [%of_sexp: elem list] sexp
  let of_queue q k = Queue.iter q ~f:k
  let of_set s k = Core.Set.iter ~f:k s
  let of_hashtbl x k = Hashtbl.iteri ~f:(fun ~key ~data -> k (key, data)) x

  let to_set (type t w) m iter =
    let module C = (val m : Comparator.S with type t = t and type comparator_witness = w)
    in
    let l = to_list iter |> List.sort ~compare:C.comparator.compare in
    Core.Set.of_list m l

  let top_k ~cmp k l f =
    assert (k >= 0);
    if k > 0 then (
      let mins = Pairing_heap.create ~min_size:k ~cmp () in
      l (fun x ->
          if
            Pairing_heap.length mins < k
            || Option.is_some
               @@ Pairing_heap.pop_if mins (fun x' ->
                      let c = cmp x' x in
                      c < 0 || (c = 0 && Random.bool ()))
          then Pairing_heap.add mins x);
      Pairing_heap.iter ~f mins)

  let%expect_test "" =
    print_s [%message (top_k ~cmp:[%compare: int] 3 Iter.(0 -- 10) : int t)];
    [%expect
      {| ("top_k ~cmp:([%compare : int]) 3 (let open Iter in 0 -- 10)" (8 10 9)) |}]

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

let ( <. ) = Float.( < )
let ( >. ) = Float.( > )
let ( <=. ) = Float.( <= )
let ( >=. ) = Float.( >= )
