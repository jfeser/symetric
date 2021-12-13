module Offset = struct
  module T = struct
    type kind = Cuboid_x | Cuboid_y | Cuboid_z | Cylinder [@@deriving compare, hash, sexp]
    type t = { id : int; kind : kind } [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)
end

module T = struct
  type t = Vector | Offset of Offset.t [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)
