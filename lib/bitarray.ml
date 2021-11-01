type buf = int array [@@deriving compare, sexp]

let hash_fold_buf = Hash.Builtin.(hash_fold_array_frozen hash_fold_int)

let hash_buf = Hash.of_fold hash_fold_buf

type t = { len : int; buf : buf } [@@deriving compare, hash, sexp]

let length t = t.len

let bits_per_word = 63

let nwords len = (len / bits_per_word) + if len mod bits_per_word > 0 then 1 else 0

let create len v =
  let fill = if v then 0x7FFFFFFFFFFFFFFF else 0 in
  { len; buf = Array.create ~len:(nwords len) fill }

let init_fold ~f ~init len =
  let nwords = nwords len in
  let buf = Array.create ~len:nwords 0 in
  let idx = ref 0 in
  let state = ref init in
  for w = 0 to nwords - 1 do
    for b = 0 to bits_per_word - 1 do
      if !idx < len then (
        let state', value = f !state !idx in
        let v = if value then 1 else 0 in
        buf.(w) <- buf.(w) + (v lsl b);
        incr idx;
        state := state')
    done
  done;
  { len; buf }

let init ~f len = init_fold ~f:(fun () i -> ((), f i)) ~init:() len

let%expect_test "init-in-bounds" =
  let len = 99 in
  (init len ~f:(fun i ->
       [%test_pred: int] (fun i -> 0 <= i && i < len) i;
       false)
    : t)
  |> ignore

let of_list x =
  init_fold (List.length x) ~f:(fun xs _ -> match xs with x :: xs' -> (xs', x) | [] -> assert false) ~init:x

let get t i = (t.buf.(i / bits_per_word) lsr (i mod bits_per_word)) land 1 > 0

let to_list x = List.init (length x) ~f:(get x)

let fold { len; buf } ~f ~init =
  let i = ref 0 and n_words = Array.length buf and state = ref init in
  for w = 0 to n_words - 2 do
    let word = buf.(w) in
    for b = 0 to bits_per_word - 1 do
      let bit = (word lsr b) land 1 > 0 in
      state := f !state bit;
      incr i
    done
  done;
  let last_word = buf.(n_words - 1) in
  for b = 0 to len - !i - 1 do
    let bit = (last_word lsr b) land 1 > 0 in
    assert (!i < len);
    state := f !state bit;
    incr i
  done;
  !state

let iteri x ~f =
  (fold x
     ~f:(fun i x ->
       f i x;
       i + 1)
     ~init:0
    : int)
  |> ignore

let%expect_test "iteri-in-bounds" =
  let len = 99 in
  create len false |> iteri ~f:(fun i _ -> [%test_pred: int] (fun i -> 0 <= i && i < len) i)

let not a = { a with buf = Array.map a.buf ~f:lnot }

let and_ a b = { len = a.len; buf = Array.map2_exn a.buf b.buf ~f:( land ) }

let or_ a b = { len = a.len; buf = Array.map2_exn a.buf b.buf ~f:( lor ) }

let xor a b = { len = a.len; buf = Array.map2_exn a.buf b.buf ~f:( lxor ) }

let hamming_weight x = Array.sum (module Int) x.buf ~f:Int.popcount

let hamming_distance a b =
  assert (Array.length a.buf = Array.length b.buf);
  let sum = ref 0 in
  for i = 0 to Array.length a.buf - 1 do
    sum := !sum + Int.popcount (a.buf.(i) lxor b.buf.(i))
  done;
  !sum

let%expect_test "" =
  for _ = 0 to 100 do
    let l = List.init ~f:(fun _ -> Random.bool ()) 256 in
    [%test_result: bool list] ~expect:l (of_list l |> to_list)
  done

let%expect_test "" =
  for i = 0 to 10 do
    let b = init 10 ~f:(fun j -> j < i) in
    [%test_result: int] ~expect:i (hamming_weight b)
  done

let to_ndarray x =
  let len = length x and bit_get = get in
  Owl.Arr.init [| len |] (fun i -> if bit_get x i then 1.0 else 0.0)

let to_torch x =
  let len = length x and bit_get = get in
  Torch.Tensor.of_float1 ~device:Torch_core.Device.Cpu @@ Array.init len ~f:(fun i -> if bit_get x i then 1.0 else 0.0)
