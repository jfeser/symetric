module T = struct
  type t = { lo : int; hi : int } [@@deriving compare, equal, hash, sexp]

  module Elt = struct
    type t = int [@@deriving compare, equal, hash, sexp]
  end

  let fold r ~init ~f =
    let rec fold i acc = if i < r.hi then fold (i + 1) (f acc i) else acc in
    fold r.lo init

  let iter r ~f =
    let rec iter i =
      if i < r.hi then (
        f i;
        iter (i + 1))
    in
    iter r.lo

  let iter = `Custom iter
  let length r = r.hi - r.lo
  let length = `Custom length
end

include T
include Container.Make0 (T)

let create lo hi = { lo; hi }
let mem { lo; hi } x = lo <= x && x < hi
