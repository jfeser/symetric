open Std

module Size = struct
  type t = { xres : int; yres : int } [@@deriving compare, hash, sexp]

  let create ~xres ~yres () =
    assert (xres > 0 && yres > 0);
    { xres; yres }

  let offset s x y = ((s.yres - 1 - y) * s.xres) + x

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
      [%test_result: int] (Gen.length @@ pixels @@ create ~xres ~yres ()) ~expect:(xres * yres)
    done
end

module T = struct
  include Hash_cached.Make (Bitarray)
end

include T
include Comparator.Make (T)

let[@inline] pixels x = value x

let init (size : Size.t) ~f =
  let len = size.xres * size.yres in
  let pixels = Size.pixels size in
  Bitarray.init len ~f:(fun idx ->
      let ptx, pty = Option.value_exn (pixels ()) in
      f idx ptx pty)
  |> create

let npixels s = Bitarray.length @@ pixels s

let to_iter (size : Size.t) s f =
  let pixel_gen = Size.pixels size in
  Bitarray.iteri (pixels s) ~f:(fun idx v -> f (Option.value_exn (pixel_gen ()), v))

let pp fmt ((size : Size.t), x) =
  Bitarray.iteri (pixels x) ~f:(fun i b ->
      if b then Fmt.pf fmt "█" else Fmt.pf fmt ".";
      if i mod size.xres = size.xres - 1 then Fmt.pf fmt "\n")

let get x = Bitarray.get (pixels x)
let[@inline] hamming c c' = Bitarray.hamming_distance (pixels c) (pixels c')
let is_empty s = Bitarray.is_empty (pixels s)

let jaccard c c' =
  let h = hamming c c' in
  let l = Bitarray.length (pixels c) in
  Float.(of_int h / of_int l)

let inter c c' =
  let c = pixels c and c' = pixels c' in
  let union = Bitarray.(Float.of_int @@ hamming_weight (or_ c c')) in
  if Float.(union = 0.0) then 1.0 else 1.0 -. Bitarray.(Float.((of_int @@ hamming_weight (and_ c c')) /. union))

let shift (size : Size.t) s dx dy =
  init size ~f:(fun _ x y ->
      let x' = x - dx and y' = y - dy in
      if x' < 0 || x' >= size.xres || y' < 0 || y' >= size.yres then false else get s @@ Size.offset size x' y')

let crop ~old:(size : Size.t) ~new_:(size' : Size.t) s = init size' ~f:(fun _ x y -> get s @@ Size.offset size x y)

let edges (size : Size.t) s =
  let get x y = if x < 0 || x >= size.xres || y < 0 || y >= size.yres then false else get s @@ Size.offset size x y in
  init size ~f:(fun i x y -> get x y && not (get (x - 1) y && get (x + 1) y && get x (y - 1) && get x (y + 1)))

let corners (size : Size.t) s =
  let pix = pixels s in
  let get x y = x >= 0 && x < size.xres && y >= 0 && y < size.yres && (Bitarray.get pix @@ Size.offset size x y) in
  let above x y = get x (y + 1)
  and below x y = get x (y - 1)
  and left x y = get (x - 1) y
  and right x y = get (x + 1) y in
  let c1 = init size ~f:(fun _ x y -> get x y && (not @@ above x y) && (not @@ left x y))
  and c2 = init size ~f:(fun _ x y -> get x y && (not @@ above x y) && (not @@ right x y))
  and c3 = init size ~f:(fun _ x y -> get x y && (not @@ below x y) && (not @@ left x y))
  and c4 = init size ~f:(fun _ x y -> get x y && (not @@ below x y) && (not @@ right x y)) in
  [ c1; c2; c3; c4 ]

let count s = Bitarray.hamming_weight (pixels s)

let distance (size : Size.t) s s' =
  let dist = ref 0.0 in

  for x = 0 to size.xres - 1 do
    for y = 0 to size.yres - 1 do
      for x' = 0 to size.xres - 1 do
        for y' = 0 to size.yres - 1 do
          let x1 = get s (Size.offset size x y)
          and y1 = get s' (Size.offset size x y)
          and x2 = get s (Size.offset size x' y')
          and y2 = get s' (Size.offset size x' y') in
          let to_float' x = if x then 1.0 else 0.0 in
          let sum =
            Float.(
              (to_float' x1 -. to_float' y1)
              *. (0.6 ** (abs (of_int x - of_int x') + abs (of_int y - of_int y')))
              *. (to_float' x2 - to_float' y2))
          in
          dist := !dist +. sum
        done
      done
    done
  done;
  !dist
