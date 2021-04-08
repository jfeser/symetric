type t = { len : int; buf : string } [@@deriving compare, hash, sexp]

let length t = t.len

let init ~f len =
  let nchars = (len / 8) + if len mod 8 > 0 then 1 else 0 in
  let buf =
    String.init nchars ~f:(fun c ->
        let char = ref 0 in
        for j = 0 to 7 do
          let idx = (c * 8) + j in
          let v = if idx < len && f idx then 1 else 0 in
          char := !char + (v lsl j)
        done;
        char_of_int !char)
  in
  { len; buf }

let of_list x =
  let x = List.to_array x in
  init (Array.length x) ~f:(Array.get x)

let get t i =
  [%test_pred: int * int * int] ~message:"index out of bounds"
    (fun (lo, x, hi) -> lo <= x && x < hi)
    (0, i, length t);
  let ch = int_of_char t.buf.[i lsr 3] in
  let mask = 1 lsl (i land 7) in
  ch land mask <> 0

let to_list x = List.init (length x) ~f:(get x)

let char_of_int_mask i = char_of_int (i land 0xff)

let not a =
  {
    a with
    buf =
      String.map a.buf ~f:(fun c -> char_of_int_mask @@ lnot @@ int_of_char c);
  }

let and_ a b =
  [%test_result: int] ~expect:a.len b.len;
  {
    len = a.len;
    buf =
      String.mapi a.buf ~f:(fun i ac ->
          char_of_int_mask @@ (int_of_char ac land int_of_char b.buf.[i]));
  }

let or_ a b =
  [%test_result: int] ~expect:a.len b.len;
  {
    len = a.len;
    buf =
      String.mapi a.buf ~f:(fun i ac ->
          char_of_int_mask @@ (int_of_char ac lor int_of_char b.buf.[i]));
  }

let xor a b =
  [%test_result: int] ~expect:a.len b.len;
  {
    len = a.len;
    buf =
      String.mapi a.buf ~f:(fun i ac ->
          char_of_int_mask @@ (int_of_char ac lxor int_of_char b.buf.[i]));
  }

let equal a b = not (xor a b)

let all a =
  let ret = ref true in
  for i = 0 to length a - 1 do
    ret := !ret && get a i
  done;
  !ret
