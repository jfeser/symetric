open! Core

include Hash_set.Make (struct
  module T = struct
    let hash_fold_array f state a = Array.fold a ~init:state ~f

    type t = int array list [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end)
