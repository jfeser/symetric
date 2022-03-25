module Make_sexp_of (T : sig
  type t [@@deriving compare, hash, sexp_of]
end) =
struct
  module S = struct
    type t = { value : T.t; hash : int }

    let compare x x' = [%compare: T.t] x.value x'.value
    let sexp_of_t x = [%sexp_of: T.t] x.value
  end

  include S
  include Comparator.Make (S)

  let create x = { value = x; hash = [%hash: T.t] x }
  let[@inline] value x = x.value
  let hash x = x.hash
  let hash_fold_t s x = [%hash_fold: int] s @@ hash x

  let compare x x' =
    let hcmp = [%compare: int] (hash x) (hash x') in
    if hcmp = 0 then [%compare: T.t] (value x) (value x') else hcmp

  let equal x x' = hash x = hash x' && [%compare.equal: T.t] (value x) (value x')
  let sexp_of_t x = [%sexp_of: T.t] (value x)
end

module Make (T : sig
  type t [@@deriving compare, hash, sexp]
end) =
struct
  include Make_sexp_of (T)

  let t_of_sexp x = create @@ [%of_sexp: T.t] x
end
