module T = struct
  module T0 = struct
    type t = { pixels : Bitarray.t; xlen : int; ylen : int }
    [@@deriving compare, hash, sexp]
  end

  include Hash_cached.Make (T0)
end

module C = Comparator.Make (T)

module TC = struct
  include T
  include C
end

include TC

let create ~xlen ~ylen pixels = create { xlen; ylen; pixels }

let xlen x = (value x).xlen

let ylen x = (value x).ylen

let pixels x = (value x).pixels

let copy ?xlen:xlen_ ?ylen:ylen_ ?pixels:pixels_ x =
  create
    ~xlen:(Option.value xlen_ ~default:(xlen x))
    ~ylen:(Option.value ylen_ ~default:(ylen x))
    (Option.value pixels_ ~default:(pixels x))
