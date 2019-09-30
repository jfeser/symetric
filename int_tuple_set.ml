open! Core

module Elt = Trace.Make (struct
  module T = struct
    type t = int [@@deriving compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)
end)

include Hash_set.Make (Elt)
