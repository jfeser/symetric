open! Core

module Make (T : sig
  type t [@@deriving compare, hash, sexp]
end) =
struct
  module T = struct
    type t = { value : T.t; sizes : int list } [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end
