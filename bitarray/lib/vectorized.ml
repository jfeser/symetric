open! Core

type buf = string [@@deriving compare, hash, sexp]
type t = { buf : buf; len : int } [@@deriving compare, hash, sexp]

let[@inline] ceil_div x y = (x + y - 1) / y
let[@inline] length x = x.len
let bits_per_word = 8
let bits_per_vword = 32
let[@inline] nwords len = ceil_div len bits_per_vword * (bits_per_vword / bits_per_word)
let[@inline] vwords t = String.length t.buf / (bits_per_vword / bits_per_word)
let[@inline] create_buf len = Bytes.make (nwords len) '\x00'

let create len init =
  let nwords = nwords len in
  let init_word = if init then '\xFF' else '\x00' in
  let buf = String.make nwords init_word in
  { buf; len }

external bitarray_and : string -> string -> bytes -> (int[@untagged]) -> unit = "" "bitarray_and_stub" [@@noalloc]
external bitarray_or : string -> string -> bytes -> (int[@untagged]) -> unit = "" "bitarray_or_stub" [@@noalloc]
external bitarray_xor : string -> string -> bytes -> (int[@untagged]) -> unit = "" "bitarray_xor_stub" [@@noalloc]
external bitarray_any : string -> (int[@untagged]) -> bool = "" "bitarray_any_stub" [@@noalloc]
external bitarray_not : string -> bytes -> (int[@untagged]) -> unit = "" "bitarray_not_stub" [@@noalloc]

external bitarray_hamming_weight : string -> (int[@untagged]) -> (int[@untagged]) = "" "bitarray_hamming_weight_stub"
  [@@noalloc]

external bitarray_hamming_distance : string -> string -> (int[@untagged]) -> (int[@untagged])
  = "" "bitarray_hamming_distance_stub"
  [@@noalloc]

external bitarray_replicate :
  string ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  bytes ->
  (int[@untagged]) ->
  unit = "" "bitarray_replicate_stub"
  [@@noalloc]

let[@inline] binary op a b =
  assert (a.len = b.len);
  let buf = create_buf a.len in
  op a.buf b.buf buf (vwords a);
  { buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf; len = a.len }

let unary op a =
  let buf = create_buf a.len in
  op a.buf buf (vwords a);
  { buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf; len = a.len }

let and_ = binary bitarray_and
let or_ = binary bitarray_or
let xor = binary bitarray_xor
let not_ = unary bitarray_not
let hamming_weight a = bitarray_hamming_weight a.buf (vwords a)

let hamming_distance a b =
  assert (a.len = b.len);
  bitarray_hamming_distance a.buf b.buf (vwords a)

let any a = bitarray_any a.buf (vwords a)
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

let replicate ~w ~h t ~dx ~dy ~ct =
  assert (w >= 0 && h >= 0 && t.len = w * h && ct >= 0);
  let buf = create_buf t.len in
  bitarray_replicate t.buf dx dy ct w h buf (vwords t);
  { buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf; len = t.len }

let init_bitmap = Shared.init_bitmap init_fold
let pp_bitmap = Shared.pp_bitmap iteri

let%test_unit "hamming-weight" =
  for _ = 0 to 100 do
    let len = Random.int_incl 8 256 in
    let bits = List.init len ~f:(fun _ -> Random.bool ()) in
    let expect = List.sum (module Int) bits ~f:(fun b -> if b then 1 else 0) in
    let v = of_list bits in
    let actual = hamming_weight v in
    [%test_result: int] ~expect actual
  done
