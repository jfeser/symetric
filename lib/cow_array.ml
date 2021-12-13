open Core

type 'a t = 'a array [@@deriving compare, equal, sexp]

let hash_fold_t = Hash.Builtin.hash_fold_array_frozen
let get = Array.get

let set a i v =
  let a' = Array.copy a in
  a'.(i) <- v;
  a'

let of_array = Fun.id
let length = Array.length
let count = Array.count
let map2_exn = Array.map2_exn
