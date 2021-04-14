module T = struct
  type t = True | False | Maybe [@@deriving compare, hash, sexp]
end

include T

module O : Comparable.Infix with type t := t = struct
  include Comparable.Make (T)
end

let is_true = function True -> true | _ -> false

let is_false = function False -> true | _ -> false
