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
end

module Random = struct
  include Random

  let list_elem_exn l = List.nth_exn l (Random.int @@ List.length l)
end

module Iter = struct
  include Iter

  let of_queue q k = Queue.iter q ~f:k
end