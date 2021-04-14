module T = struct
  type t = { pixels : Bitarray.t; xlen : int; ylen : int }
  [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)
