type t = { len : int; buf : string } [@@deriving compare, hash, sexp]

let length t = t.len

let init ~f len =
  let buf =
    String.init len ~f:(fun c ->
        let i = c * 8 in
        let char = ref 0 in
        for j = 0 to 7 do
          if f (i + j) then char := !char + (1 lsl j)
        done;
        char_of_int !char)
  in
  { len; buf }

let of_list x =
  let x = List.to_array x in
  init (Array.length x) ~f:(Array.get x)

let get t i =
  let ch = int_of_char t.buf.[i lsr 3] in
  let mask = 1 lsl (i land 7) in
  ch land mask <> 0

let to_list x = List.init (length x) ~f:(get x)

let not a =
  {
    a with
    buf = String.map a.buf ~f:(fun c -> char_of_int @@ lnot @@ int_of_char c);
  }

let equal a b =
  [%test_result: int] ~expect:a.len b.len;
  {
    len = a.len;
    buf =
      String.init a.len ~f:(fun i ->
          char_of_int @@ lnot
          @@ (int_of_char a.buf.[i] lxor int_of_char b.buf.[i]));
  }

let all a =
  let ret = ref true in
  for i = 0 to length a - 1 do
    ret := !ret && get a i
  done;
  !ret
