let reduce_while l ~f =
  let rec loop g gs = function
    | [] -> g :: gs
    | x :: xs -> ( match f g x with Some g' -> loop g' gs xs | None -> loop x (g :: gs) xs)
  in
  match l with [] -> [] | x :: xs -> loop x [] xs

let rec update l ~f =
  match l with
  | [] -> None
  | x :: xs -> (
      match f x with
      | Some x' -> Some (x' :: xs)
      | None -> ( match update ~f xs with Some xs' -> Some (x :: xs') | None -> None))

let rec repeat ~f i x = if i <= 0 then [ x ] else x :: repeat (i - 1) ~f (f x)

let unsafe_to_list a = List.init (Option_array.length a) ~f:(Option_array.unsafe_get_some_assuming_some a)

module Option = struct
  include Option

  let value_lazy ~default = function Some x -> x | None -> Lazy.force default
end

module List = struct
  include List

  let unzip4 list =
    let rec loop list l1 l2 l3 l4 =
      match list with
      | [] -> (rev l1, rev l2, rev l3, rev l4)
      | (x, y, z, a) :: tl -> loop tl (x :: l1) (y :: l2) (z :: l3) (a :: l4)
    in
    loop list [] [] [] []

  let insert_sorted ~compare x xs =
    let rec insert = function [] -> [ x ] | y :: ys -> if compare x y <= 0 then x :: y :: ys else y :: insert ys in
    insert xs

  let product l = List.reduce_exn l ~f:( * )

  let group_by m l = Hashtbl.of_alist_multi m l |> Hashtbl.to_alist
end

module Random = struct
  include Random

  let list_elem_exn l = List.nth_exn l (Random.int @@ List.length l)
end

module Iter = struct
  include Iter

  let sexp_of_t sexp_of_elem iter = [%sexp_of: elem list] @@ Iter.to_list iter

  let of_queue q k = Queue.iter q ~f:k

  let of_set s k = Core.Set.iter ~f:k s

  let to_set (type t w) m iter =
    let module C = (val m : Comparator.S with type t = t and type comparator_witness = w) in
    let l = to_list iter |> List.sort ~compare:C.comparator.compare in
    Core.Set.of_list m l

  let top_k (type t) ~cmp k (l : t Iter.t) (f : t -> unit) =
    assert (k >= 0);
    if k > 0 then (
      let mins = Pairing_heap.create ~min_size:k ~cmp () in
      Iter.iter
        (fun (x : t) ->
          if Pairing_heap.length mins < k || (Option.is_some @@ Pairing_heap.pop_if mins (fun x' -> cmp x' x < 0)) then
            Pairing_heap.add mins x)
        l;
      Pairing_heap.iter ~f mins)

  let%expect_test "" =
    print_s [%message (top_k ~cmp:[%compare: int] 3 Iter.(0 -- 10) : int t)];
    [%expect {| ("top_k ~cmp:([%compare : int]) 3 (let open Iter in 0 -- 10)" (8 10 9)) |}]

  let list_product iters f =
    let rec product acc = function [] -> f @@ List.rev acc | q :: qs -> q (fun x -> product (x :: acc) qs) in
    product [] iters

  let%expect_test "" =
    print_s [%message (list_product [ Iter.(1 -- 1) ] : int list t)];
    [%expect {|
      ("list_product [(let open Iter in 1 -- 1)]" ((1))) |}]

  let%expect_test "" =
    print_s [%message (list_product [ Iter.(1 -- 3); Iter.(1 -- 3); Iter.(1 -- 3) ] : int list t)];
    [%expect
      {|
      ( "list_product\
       \n  [(let open Iter in 1 -- 3);\
       \n  (let open Iter in 1 -- 3);\
       \n  (let open Iter in 1 -- 3)]"
       ((1 1 1) (1 1 2) (1 1 3) (1 2 1) (1 2 2) (1 2 3) (1 3 1) (1 3 2) (1 3 3)
        (2 1 1) (2 1 2) (2 1 3) (2 2 1) (2 2 2) (2 2 3) (2 3 1) (2 3 2) (2 3 3)
        (3 1 1) (3 1 2) (3 1 3) (3 2 1) (3 2 2) (3 2 3) (3 3 1) (3 3 2) (3 3 3))) |}]
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

  let inversions compare a =
    let inv = ref 0 in
    for i = 0 to Array.length a - 2 do
      if compare a.(i) a.(i + 1) > 0 then incr inv
    done;
    !inv

  let median compare a =
    let a' = Array.sorted_copy a ~compare in
    a'.(Array.length a' / 2)
end
