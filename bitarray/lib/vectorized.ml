open! Core

type buf = string [@@deriving compare, hash, sexp]
type t = { buf : buf; len : int } [@@deriving compare, hash, sexp]

let[@inline] ceil_div x y = (x + y - 1) / y
let[@inline] length x = x.len
let bits_per_word = 8
let[@inline] nwords len = ceil_div len 64 * (64 / bits_per_word)
let[@inline] create_buf len = Bytes.make (nwords len) '\x00'

let create len init =
  let nwords = nwords len in
  let init_word = if init then '\xFF' else '\x00' in
  let buf = String.make nwords init_word in
  { buf; len }

external bitarray_and : buf -> buf -> buf -> (int[@untagged]) -> unit = "" "bitarray_and_stub" [@@noalloc]
external bitarray_or : buf -> buf -> buf -> (int[@untagged]) -> unit = "" "bitarray_or_stub" [@@noalloc]
external bitarray_xor : buf -> buf -> buf -> (int[@untagged]) -> unit = "" "bitarray_xor_stub" [@@noalloc]
external bitarray_any : buf -> (int[@untagged]) -> bool = "" "bitarray_any_stub" [@@noalloc]
external bitarray_not : buf -> buf -> (int[@untagged]) -> unit = "" "bitarray_not_stub" [@@noalloc]

external bitarray_hamming_weight : buf -> (int[@untagged]) -> (int[@untagged]) = "" "bitarray_hamming_weight_stub"
  [@@noalloc]

external bitarray_hamming_distance : buf -> buf -> (int[@untagged]) -> (int[@untagged])
  = "" "bitarray_hamming_distance_stub"
  [@@noalloc]

external bitarray_replicate :
  buf ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  buf ->
  (int[@untagged]) ->
  unit = "" "bitarray_replicate_stub"
  [@@noalloc]

let and_ a b =
  assert (a.len = b.len);
  let c = create a.len false in
  bitarray_and a.buf b.buf c.buf (a.len / 64);
  c

let or_ a b =
  assert (a.len = b.len);
  let c = create a.len false in
  bitarray_or a.buf b.buf c.buf (a.len / 64);
  c

let xor a b =
  assert (a.len = b.len);
  let c = create a.len false in
  bitarray_xor a.buf b.buf c.buf (a.len / 64);
  c

let not_ a =
  let r = create a.len false in
  bitarray_not a.buf r.buf (a.len / 64);
  r

let hamming_weight a = bitarray_hamming_weight a.buf (a.len / 64)

let%test_unit "hamming-weight" =
  [%test_result: int] ~expect:(bits_per_word * 4) (hamming_weight @@ create 4 true);
  [%test_result: int] ~expect:0 (hamming_weight @@ create 4 false)

let hamming_distance a b =
  assert (a.len = b.len);
  bitarray_hamming_distance a.buf b.buf (a.len / 64)

let any a = bitarray_any a.buf (a.len / 64)
let none a = not (any a)
let is_empty = none
let[@inline] read_bit w b = (Char.to_int w lsr b) land 1 > 0

let fold { len; buf } ~f ~init =
  let i = ref 0 and n_words = ceil_div len bits_per_word and state = ref init in
  for w = 0 to n_words - 2 do
    let word = buf.[w] in
    for b = 0 to bits_per_word - 1 do
      state := f !state (read_bit word b);
      incr i
    done
  done;
  let last_word = buf.[n_words - 1] in
  for b = 0 to len - !i - 1 do
    state := f !state (read_bit last_word b);
    incr i
  done;
  !state

let init_fold ~f ~init len =
  let buf = create_buf len in
  let nwords = ceil_div len bits_per_word in
  let i = ref 0 and state = ref init in
  for w = 0 to nwords - 2 do
    let x = ref 0 in
    for b = 0 to bits_per_word - 1 do
      let state', value = f !state !i in
      x := !x + (Bool.to_int value lsl b);
      incr i;
      state := state'
    done;
    Bytes.set buf w @@ Char.of_int_exn !x
  done;
  let x = ref 0 in
  for b = 0 to len - !i - 1 do
    let state', value = f !state !i in
    x := !x + (Bool.to_int value lsl b);
    incr i;
    state := state'
  done;
  Bytes.set buf (nwords - 1) @@ Char.of_int_exn !x;
  { buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf; len }

let get t i =
  let w = i / bits_per_word and b = i mod bits_per_word in
  read_bit t.buf.[w] b

let init ~f x = Shared.init ~init_fold ~f x
let iteri x ~f = Shared.iteri ~fold x ~f
let of_list x = Shared.of_list ~init_fold x
let to_list x = List.init (length x) ~f:(get x)
let not = not_

let weighted_jaccard ?(pos_weight = 0.5) a b =
  ((Float.of_int (hamming_weight (not @@ and_ a b)) *. pos_weight)
  +. (Float.of_int (hamming_weight (not @@ and_ (not a) (not b))) *. (1.0 -. pos_weight)))
  /. (Float.of_int @@ length a)

(* let[@inline] set_word t i x = t.buf.{i} <- x *)
(* let[@inline] get_word t i = t.buf.{i} *)

(* let shift_right x n = *)
(*   assert (n >= 0); *)
(*   if n = 0 then x *)
(*   else if n >= bits_per_word * length x then create (length x) false *)
(*   else *)
(*     let word_offset = n / bits_per_word and bit_offset = n mod bits_per_word in *)
(*     let ret = create (length x) false in *)
(*     for i = 0 to word_offset - 1 do *)
(*       set_word ret i 0L *)
(*     done; *)
(*     set_word ret word_offset Int64.(get_word x 0 lsr bit_offset); *)
(*     for i = word_offset + 1 to length ret - 1 do *)
(*       let high_word = get_word x (i - word_offset - 1) in *)
(*       let low_word = get_word x (i - word_offset) in *)
(*       let ls = bits_per_word - bit_offset in *)
(*       set_word ret i Int64.((high_word lsl ls) lor (low_word lsr bit_offset)) *)
(*     done; *)
(*     ret *)

(* let%test_unit "shift-right" = *)
(*   let x = create 4 true in *)
(*   for i = 0 to bits_per_word * 4 do *)
(*     [%test_result: int] ~expect:((bits_per_word * 4) - i) (hamming_weight (shift_right x i)) *)
(*   done *)

(* let shift_left x n = *)
(*   assert (n >= 0); *)
(*   if n = 0 then x *)
(*   else if n >= bits_per_word * length x then create (length x) false *)
(*   else *)
(*     let word_offset = n / bits_per_word and bit_offset = n mod bits_per_word in *)
(*     let ret = create (length x) false in *)
(*     for i = 0 to word_offset - 1 do *)
(*       set_word ret i 0L *)
(*     done; *)
(*     set_word ret word_offset Int64.(get_word x 0 lsr bit_offset); *)
(*     for i = word_offset + 1 to length ret - 1 do *)
(*       let high_word = get_word x (i - word_offset - 1) in *)
(*       let low_word = get_word x (i - word_offset) in *)
(*       let ls = bits_per_word - bit_offset in *)
(*       set_word ret i Int64.((high_word lsl ls) lor (low_word lsr bit_offset)) *)
(*     done; *)
(*     ret *)

let replicate ~w ~h t ~dx ~dy ~ct =
  let t' = create t.len false in
  bitarray_replicate t.buf dx dy ct w h t'.buf t.len;
  t'

let init_bitmap = Shared.init_bitmap init_fold
let pp_bitmap = Shared.pp_bitmap iteri
