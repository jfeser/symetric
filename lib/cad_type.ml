module T = struct
  type t = Scene [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)
