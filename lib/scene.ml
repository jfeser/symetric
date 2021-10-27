module Size = struct
  type t = { xres : int; yres : int; xlen : float; ylen : float } [@@deriving compare, hash, sexp]

  let create ?xlen ?ylen ~xres ~yres () =
    let xlen = Option.value xlen ~default:(Float.of_int xres)
    and ylen = Option.value ylen ~default:(Float.of_int yres) in
    assert (xres >= 0 && yres >= 0 && Float.(xlen >= 0.0 && ylen >= 0.0));
    { xres; yres; xlen; ylen }

  (** Normalize point to [0, 1] *)
  let norm s (v : Vector2.t) : Vector2.t = { x = v.x /. s.xlen; y = v.y /. s.xlen }

  (** Get offset of pixel containing point. Note pixels are stored left to right
     starting from the top. *)
  let offset_of_pt s v =
    let nv = norm s v in
    let x = Float.iround_down_exn (nv.x *. Float.of_int s.xres)
    and y = Float.iround_down_exn (nv.y *. Float.of_int s.yres) in
    ((s.yres - 1 - y) * s.xres) + x
end

module T = struct
  include Hash_cached.Make (Bitarray)
end

include T
include Comparator.Make (T)

let pixels = value

let init (size : Size.t) ~f =
  let len = size.xres * size.yres in
  let pixel_w = Float.(size.xlen / of_int size.xres) and pixel_h = Float.(size.ylen / of_int size.yres) in
  let px = ref 0 and ptx = ref (pixel_w *. 0.5) and pty = ref (size.ylen -. (pixel_h *. 0.5)) in
  Bitarray.init len ~f:(fun idx ->
      let ret = f idx !ptx !pty in
      (* update pixel position *)
      if !px = size.xres - 1 then (
        px := 0;
        ptx := pixel_w *. 0.5;
        pty := !pty -. pixel_h)
      else (
        incr px;
        ptx := !ptx +. pixel_w);
      ret)
  |> create

let pp fmt ((size : Size.t), x) =
  Bitarray.iter (pixels x) ~f:(fun i b ->
      if b then Fmt.pf fmt "█" else Fmt.pf fmt ".";
      if i mod size.xres = size.xres - 1 then Fmt.pf fmt "\n")

let get x = Bitarray.get (pixels x)

let hamming c c' = Bitarray.hamming_distance (pixels c) (pixels c')

let jaccard c c' =
  let h = hamming c c' in
  let l = Bitarray.length (pixels c) in
  Float.(of_int h / of_int l)
