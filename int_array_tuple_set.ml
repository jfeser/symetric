open! Core

module Elt = Trace.Make (struct
  module T = struct
    let hash_fold_array f state a = Array.fold a ~init:state ~f

    type t = int array [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end)

include Hash_set.Make (Elt)
