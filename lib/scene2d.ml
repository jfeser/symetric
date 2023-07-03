module Dim = struct
  type t = { xres : int; yres : int; scaling : int }
  [@@deriving compare, equal, hash, sexp, yojson]

  let[@inline] xres x = x.xres
  let[@inline] yres x = x.xres
  let scaled_xres x = x.xres * x.scaling
  let scaled_yres x = x.yres * x.scaling
  let default_scaling = 2
  let default_xres = 16
  let default_yres = 16

  let create ?(scaling = default_scaling) ?(xres = default_xres) ?(yres = default_yres) ()
      =
    assert (xres > 0 && yres > 0 && scaling >= 1);
    { xres; yres; scaling }

  let npixels dim = dim.xres * dim.scaling * (dim.yres * dim.scaling)
  let offset s x y = (((s.yres * s.scaling) - 1 - y) * (s.xres * s.scaling)) + x
  let ( = ) = [%compare.equal: t]

  let param =
    let open Command.Let_syntax in
    [%map_open
      let scene_width =
        flag "-scene-width"
          (optional_with_default default_xres int)
          ~doc:" scene width in pixels"
      and scene_height =
        flag "-scene-height"
          (optional_with_default default_yres int)
          ~doc:" scene height in pixels"
      and scaling =
        flag "-scaling"
          (optional_with_default default_scaling int)
          ~doc:" scene scaling factor"
      in
      create ~scaling ~xres:scene_width ~yres:scene_height ()]
end

module T = struct
  module H = Hash_cached.Make (Bitarray)

  type t = { dim : Dim.t; buf : H.t } [@@deriving compare, equal, hash, sexp]
end

include T
include Comparator.Make (T)

let of_bitarray dim buf =
  assert (Bitarray.length buf = Dim.npixels dim);
  { dim; buf = H.create buf }

let[@inline] pixels x = x.buf.T.H.value
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

(* let to_iter (size : Dim.t) s f = *)
(*   let pixel_gen = Dim.pixels size in *)
(*   Bitarray.iteri (pixels s) ~f:(fun _ v -> f (Option.value_exn (pixel_gen ()), v)) *)

let pp fmt x =
  let xres = Dim.scaled_xres x.dim in
  Fmt.pf fmt "@[<v>";
  Bitarray.iteri (pixels x) ~f:(fun i b ->
      if b then Fmt.pf fmt "X" else Fmt.pf fmt ".";
      if i mod xres = xres - 1 then Fmt.pf fmt "@,");
  Fmt.pf fmt "@]"

let get x = Bitarray.get (pixels x)
let[@inline] hamming c c' = Bitarray.hamming_distance (pixels c) (pixels c')
let[@inline] hamming_weight c = Bitarray.hamming_weight (pixels c)
let is_empty s = not (Bitarray.any (pixels s))

let[@inline] jaccard_canvas c c' =
  let h = hamming c c' in
  let l = Bitarray.length (pixels c) in
  Float.(of_int h / of_int l)

let[@inline] jaccard_pixels c c' =
  Bitarray.jaccard_distance c.buf.T.H.value c'.buf.T.H.value

let cosine c c' =
  let n = Bitarray.hamming_weight Bitarray.O.(pixels c land pixels c') in
  let d = Bitarray.hamming_weight (pixels c) * Bitarray.hamming_weight (pixels c') in
  if d = 0 then 0. else Float.(of_int n / of_int d)

let empty dim = init dim ~f:(fun _ _ _ -> false)

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
  of_bitarray s.dim @@ Bitarray.O.(pixels s land pixels s')

let union s s' =
  assert (Dim.(s.dim = s'.dim));
  of_bitarray s.dim @@ Bitarray.O.(pixels s lor pixels s')

let sub s s' =
  assert (Dim.(s.dim = s'.dim));
  of_bitarray s.dim @@ Bitarray.O.(pixels s land lnot (pixels s'))

let not s = of_bitarray s.dim @@ Bitarray.O.(lnot (pixels s))

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

let bitarray_corners s =
  let w = Dim.scaled_xres s.dim and h = Dim.scaled_yres s.dim in
  Bitarray.(O.(corners ~w ~h (pixels s) lor corners ~w ~h (lnot (pixels s))))

let corners s = of_bitarray s.dim @@ bitarray_corners s

let%expect_test "" =
  Fmt.pr "%a" pp (corners (rect (Dim.create ~xres:16 ~yres:16 ()) 2 2 8 8));
  [%expect
    {|
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ..X.....X.......
    ................
    ................
    ................
    ................
    ................
    ..X.....X.......
    ................
    ................ |}];
  Fmt.pr "%a" pp (corners (circle (Dim.create ~xres:16 ~yres:16 ()) 8 8 5));
  [%expect
    {|
    ................
    ................
    ................
    .....XX...XX....
    ....XX.....XX...
    ....X.......X...
    ................
    ................
    ................
    ....X.......X...
    ....XX.....XX...
    .....XX...XX....
    ................
    ................
    ................
    ................ |}];
  Fmt.pr "%a" pp
    (corners
       (not
          (sub
             (rect (Dim.create ~xres:16 ~yres:16 ()) 2 2 8 8)
             (rect (Dim.create ~xres:16 ~yres:16 ()) 4 4 6 6))));
  [%expect
    {|
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ..X.....X.......
    ................
    ....X.X.........
    ................
    ....X.X.........
    ................
    ..X.....X.......
    ................
    ................ |}]

let corner_overlap s s' =
  Bitarray.(any @@ O.(bitarray_corners s land bitarray_corners s'))

let%test_unit "" =
  let s = circle (Dim.create ~xres:16 ~yres:16 ()) 8 8 5 in
  assert (corner_overlap s s)

let ( = ) = [%compare.equal: t]
