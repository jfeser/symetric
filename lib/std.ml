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
end

module Array = struct
  include Array

  let inversions compare a =
    let inv = ref 0 in
    for i = 0 to Array.length a - 2 do
      if compare a.(i) a.(i + 1) > 0 then incr inv
    done;
    !inv
end