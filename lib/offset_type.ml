type kind = Cuboid_x | Cuboid_y | Cuboid_z | Cylinder
[@@deriving compare, hash, sexp]

module T = struct
  type t = { id : int; kind : kind } [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)
