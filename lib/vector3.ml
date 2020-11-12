open! Core

module T = struct
  type t = { x : float; y : float; z : float } [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

type dim = X | Y | Z [@@deriving compare, hash, sexp]

let l2_dist { x; y; z } { x = x'; y = y'; z = z' } =
  Float.(sqrt ((x - x') ** 2.0) + ((y - y') ** 2.0) + ((z - z') ** 2.0))

let inverse_rotate ~theta:{ x = rot_x; y = rot_y; z = rot_z } { x; y; z } =
  let open Float in
  let x1 = (cos (-rot_z) * x) - (sin (-rot_z) * y)
  and y1 = (sin (-rot_z) * x) + (cos (-rot_z) * y)
  and z1 = z in
  let x2 = (cos (-rot_y) * x1) + (sin (-rot_y) * z1)
  and y2 = y1
  and z2 = (-sin (-rot_y) * x1) + (cos (-rot_y) * z1) in
  let x3 = x2
  and y3 = (cos (-rot_x) * y2) - (sin (-rot_x) * z2)
  and z3 = (sin (-rot_x) * y2) + (cos (-rot_x) * z2) in
  { x = x3; y = y3; z = z3 }

let get dim { x; y; z } = match dim with X -> x | Y -> y | Z -> z
