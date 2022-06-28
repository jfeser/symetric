open Core

type t = string [@@deriving compare, equal, hash, sexp]

let get x i = Char.to_int x.[i]

let set x i v =
  let x = Bytes.of_string x in
  Bytes.set x i (Char.of_int_exn v);
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:x
