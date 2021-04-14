module T = struct
  type t = { pixels : Bitarray.t; xlen : int; ylen : int }
  [@@deriving compare, hash, sexp]
end

module C = Comparator.Make (T)

module TC = struct
  include T
  include C
end

include TC
