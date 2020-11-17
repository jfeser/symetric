module A = Option_array

type 'a t = 'a A.t ref

let create n = ref (A.create ~len:n)

let reserve a n =
  let new_len = Int.ceil_pow2 (n + 1) and old_len = A.length !a in
  if old_len < new_len then (
    let a' = A.create ~len:new_len in
    A.blit ~src:!a ~src_pos:0 ~len:old_len ~dst:a' ~dst_pos:0;
    a := a' )

let get_some_exn a = A.get_some_exn !a

let set_some a = A.set_some !a

let get a = A.get !a
