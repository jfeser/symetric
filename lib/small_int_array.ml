open Core

type t = string [@@deriving compare, equal, hash, sexp]

let init l ~f = String.init l ~f:(fun i -> Char.of_int_exn (f i))
let get x i = Char.to_int x.[i]

let set x i v =
  let x = Bytes.of_string x in
  Bytes.set x i (Char.of_int_exn v);
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:x

let set_many x xs =
  let ret = ref None in
  xs (fun (i, v) ->
      let buf =
        match !ret with
        | Some b -> b
        | None ->
            let b = Bytes.of_string x in
            ret := Some b;
            b
      in
      Bytes.set buf i (Char.of_int_exn v));
  match !ret with
  | Some b -> Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b
  | None -> x
