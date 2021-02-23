open! Core
open Base_quickcheck

module T = struct
  type t = { x : float; y : float } [@@deriving compare, hash, quickcheck, sexp]
end

include T
include Comparator.Make (T)

let l2_dist { x; y } { x = x'; y = y' } =
  Float.(sqrt ((x - x') ** 2.0) + ((y - y') ** 2.0))
