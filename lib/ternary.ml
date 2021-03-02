type t = True | False | Maybe [@@deriving compare, hash, sexp]

let is_true = function True -> true | _ -> false

let is_false = function False -> true | _ -> false
