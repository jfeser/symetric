open Std

module Dim = struct
  type t = { xres : int; yres : int; scaling : int }
  [@@deriving compare, hash, sexp, yojson]

  let scaled_xres x = x.xres * x.scaling
  let scaled_yres x = x.yres * x.scaling

  let create ?(scaling = 1) ~xres ~yres () =
    assert (xres > 0 && yres > 0 && scaling >= 1);
    { xres; yres; scaling }

  let npixels dim = dim.xres * dim.scaling * (dim.yres * dim.scaling)
  let offset s x y = (((s.yres * s.scaling) - 1 - y) * (s.xres * s.scaling)) + x

  (** Returns a sequence of the pixels and pixel centers for a canvas of
   `size`. Starts from the top left and continues left to right. *)
  let pixels size =
    Gen.unfold
      (fun ((px, py) as state) ->
        if py < 0 then None
        else if px = size.xres - 1 then
          let state' = (0, py - 1) in
          Some (state, state')
        else
          let state' = (px + 1, py) in
          Some (state, state'))
      (0, size.yres - 1)

  let%expect_test "" =
    for _ = 0 to 10 do
      let xres = 1 + Random.int 10 and yres = 1 + Random.int 10 in
      [%test_result: int]
        (Gen.length @@ pixels @@ create ~xres ~yres ())
        ~expect:(xres * yres)
    done

  let ( = ) = [%compare.equal: t]
end

module T = struct
  module H = Hash_cached.Make (Bitarray)

  type t = { dim : Dim.t; buf : H.t } [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

let of_bitarray dim buf =
  assert (Bitarray.length buf = Dim.npixels dim);
  { dim; buf = H.create buf }

let[@inline] pixels x = H.value x.buf
let[@inline] dim x = x.dim

let init (dim : Dim.t) ~f =
  let len = Dim.npixels dim in
  let xres = Dim.scaled_xres dim and yres = Dim.scaled_yres dim in
  let y = ref (yres - 1) and x = ref 0 in
  Bitarray.init len ~f:(fun idx ->
      assert (!y >= 0 && !y < yres && !x >= 0 && !x < xres);
      let ret = f idx !x !y in
      if !x = xres - 1 then (
        x := 0;
        decr y)
      else incr x;
      ret)
  |> of_bitarray dim

let npixels s = Bitarray.length @@ pixels s

let to_iter (size : Dim.t) s f =
  let pixel_gen = Dim.pixels size in
  Bitarray.iteri (pixels s) ~f:(fun _ v -> f (Option.value_exn (pixel_gen ()), v))

let pp fmt x =
  let xres = Dim.scaled_xres x.dim in
  Fmt.pf fmt "@[<v>";
  Bitarray.iteri (pixels x) ~f:(fun i b ->
      if b then Fmt.pf fmt "█" else Fmt.pf fmt ".";
      if i mod xres = xres - 1 then Fmt.pf fmt "@,");
  Fmt.pf fmt "@]"

let get x = Bitarray.get (pixels x)
let[@inline] hamming c c' = Bitarray.hamming_distance (pixels c) (pixels c')
let is_empty s = Bitarray.is_empty (pixels s)

let[@inline] jaccard_canvas c c' =
  let h = hamming c c' in
  let l = Bitarray.length (pixels c) in
  Float.(of_int h / of_int l)

let[@inline] jaccard_pixels c c' =
  let union = Bitarray.(hamming_weight (or_ (pixels c) (pixels c'))) in
  if union = 0 then 0.0
  else
    1.0
    -. Float.(
         of_int Bitarray.(hamming_weight (and_ (pixels c) (pixels c'))) / of_int union)

let cosine c c' =
  let n = Bitarray.hamming_weight (Bitarray.and_ (pixels c) (pixels c')) in
  let d = Bitarray.hamming_weight (pixels c) * Bitarray.hamming_weight (pixels c') in
  if d = 0 then 0. else Float.(of_int n / of_int d)

let circle (dim : Dim.t) center_x center_y radius =
  let center_x = center_x * dim.scaling
  and center_y = center_y * dim.scaling
  and radius = radius * dim.scaling in
  init dim ~f:(fun _ x y ->
      ((x - center_x) * (x - center_x)) + ((y - center_y) * (y - center_y))
      < radius * radius)

let rect (dim : Dim.t) lx ly hx hy =
  let lx = lx * dim.scaling
  and ly = ly * dim.scaling
  and hx = hx * dim.scaling
  and hy = hy * dim.scaling in
  init dim ~f:(fun _ x y -> lx <= x && ly <= y && hx >= x && hy >= y)

let inter s s' =
  assert (Dim.(s.dim = s'.dim));
  of_bitarray s.dim @@ Bitarray.and_ (pixels s) (pixels s')

let union s s' =
  assert (Dim.(s.dim = s'.dim));
  of_bitarray s.dim @@ Bitarray.or_ (pixels s) (pixels s')

let sub s s' =
  assert (Dim.(s.dim = s'.dim));
  of_bitarray s.dim @@ Bitarray.(and_ (pixels s) (not (pixels s')))

let repeat s dx dy ct =
  let dim = s.dim in
  let dx = dx * dim.scaling and dy = dy * dim.scaling in
  let xres = Dim.scaled_xres dim and yres = Dim.scaled_yres dim in
  of_bitarray s.dim @@ Bitarray.replicate ~w:xres ~h:yres (pixels s) ~dx ~dy ~ct

let translate s dx dy =
  let dim = s.dim in
  let dx = dx * dim.scaling and dy = dy * dim.scaling in
  init dim ~f:(fun _ x y ->
      let x' = x - dx and y' = y - dy in
      if x' >= 0 && x' < dim.xres && y' >= 0 && y' < dim.yres then
        Bitarray.get (pixels s) @@ Dim.offset dim x' y'
      else false)

let ( = ) = [%compare.equal: t]
