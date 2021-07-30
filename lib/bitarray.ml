type buf = int array [@@deriving compare, sexp]

let hash_fold_buf = Hash.Builtin.(hash_fold_array_frozen hash_fold_int)

let hash_buf = Hash.of_fold hash_fold_buf

type t = { len : int; buf : buf } [@@deriving compare, hash, sexp]

let length t = t.len

let bits_per_word = 63

let init ~f len =
  let nwords = (len / bits_per_word) + if len mod bits_per_word > 0 then 1 else 0 in
  let buf = Array.create ~len:nwords 0 in
  let idx = ref 0 in
  for w = 0 to nwords - 1 do
    for b = 0 to bits_per_word - 1 do
      let v = if !idx < len && f !idx then 1 else 0 in
      buf.(w) <- buf.(w) + (v lsl b);
      incr idx
    done
  done;
  { len; buf }

let of_list x =
  let x = List.to_array x in
  init (Array.length x) ~f:(Array.get x)

let get t i = (t.buf.(i / bits_per_word) lsr (i mod bits_per_word)) land 1 > 0

let to_list x = List.init (length x) ~f:(get x)

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
