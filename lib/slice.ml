open! Core

module T = struct
  type 'a t = { start : int; len : int; arr : 'a array }

  type 'a elt = 'a

  let fold x ~init ~f =
    let end_ = x.start + x.len in
    let rec fold acc i =
      if i >= end_ then acc else fold (f acc x.arr.(i)) (i + 1)
    in
    fold init x.start

  let iter x ~f =
    for i = x.start to x.start + x.len - 1 do
      f x.arr.(i)
    done

  let iter = `Custom iter

  let length x = x.len

  let length = `Custom length
end

include T

let ( .%() ) x i = x.arr.(i + x.start)

include Container.Make (T)
