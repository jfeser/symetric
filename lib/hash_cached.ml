module Make (T : sig
  type t [@@deriving compare, hash, sexp]
end) =
struct
  type t = { value : T.t; hash : int }

  let create x = { value = x; hash = [%hash: T.t] x }

  let hash x = x.hash

  let value x = x.value

  let hash_fold_t s x = [%hash_fold: int] s @@ hash x

  let compare x x' = [%compare: T.t] (value x) (value x')

  let equal x x' = hash x = hash x' && [%compare.equal: T.t] (value x) (value x')

  let sexp_of_t x = [%sexp_of: T.t] (value x)

  let t_of_sexp x = create @@ [%of_sexp: T.t] x
end
