let reduce_while l ~f =
  let rec loop g gs = function
    | [] -> g :: gs
    | x :: xs -> (
        match f g x with
        | Some g' -> loop g' gs xs
        | None -> loop x (g :: gs) xs)
  in
  match l with [] -> [] | x :: xs -> loop x [] xs

let rec update l ~f =
  match l with
  | [] -> None
  | x :: xs -> (
      match f x with
      | Some x' -> Some (x' :: xs)
      | None -> (
          match update ~f xs with Some xs' -> Some (x :: xs') | None -> None))

let rec repeat ~f i x = if i <= 0 then [ x ] else x :: repeat (i - 1) ~f (f x)

let unsafe_to_list a =
  List.init (Option_array.length a)
    ~f:(Option_array.unsafe_get_some_assuming_some a)
