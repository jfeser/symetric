open! Core

include Hash_set.Make (struct
  module T = struct
    type t = int list [@@deriving compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)
end)
