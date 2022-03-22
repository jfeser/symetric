open! Core
open Base_quickcheck

module T = struct
  type t = { x : float; y : float } [@@deriving compare, equal, hash, quickcheck, sexp]
end

include T
include Comparator.Make (T)

module O = struct
  let ( + ) a b = { x = a.x +. b.x; y = a.y +. b.y }
  let ( - ) a b = { x = a.x -. b.x; y = a.y -. b.y }
  let ( ~- ) { x; y } = { x = -.x; y = -.y }
end

let l2_dist { x; y } { x = x'; y = y' } =
  Float.(sqrt (((x - x') * (x - x')) + ((y - y') * (y - y'))))

let zero = { x = 0.0; y = 0.0 }
