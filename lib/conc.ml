module T = struct
  type t = bool array [@@deriving compare, equal, sexp]

  let hash x = [%hash: bool list] (Array.to_list x)

  let hash_fold_t s x = [%hash_fold: bool list] s (Array.to_list x)
end

include T

module O : Comparable.Infix with type t := t = struct
  include T
  include Comparable.Make (T)
end

let pp = Fmt.(array ~sep:(any " ") bool)
