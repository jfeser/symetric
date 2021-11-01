open Std

module Size = struct
  type t = { xres : int; yres : int; xlen : float; ylen : float } [@@deriving compare, hash, sexp]

  let create ?xlen ?ylen ~xres ~yres () =
    let xlen = Option.value xlen ~default:(Float.of_int xres)
    and ylen = Option.value ylen ~default:(Float.of_int yres) in
    assert (xres > 0 && yres > 0 && Float.(xlen > 0.0 && ylen > 0.0));
    { xres; yres; xlen; ylen }

  (** Normalize point to [0, 1] *)
  let norm s (v : Vector2.t) : Vector2.t = { x = v.x /. s.xlen; y = v.y /. s.xlen }

  let offset_of_pixel s x y = ((s.yres - 1 - y) * s.xres) + x

  (** Get offset of pixel containing point. Note pixels are stored left to right
     starting from the top. *)
  let offset_of_pt s v =
    let nv = norm s v in
    let x = Float.iround_down_exn (nv.x *. Float.of_int s.xres)
    and y = Float.iround_down_exn (nv.y *. Float.of_int s.yres) in
    offset_of_pixel s x y

  (** Returns a sequence of the pixels and pixel centers for a canvas of
   `size`. Starts from the top left and continues left to right. *)
  let pixel_seq size =
    let pixel_w = Float.(size.xlen / of_int size.xres) and pixel_h = Float.(size.ylen / of_int size.yres) in
    Seq.unfold
      (fun ((px, py, ptx, pty) as state) ->
        if py < 0 then None
        else if px = size.xres - 1 then
          let state' = (0, py - 1, pixel_w *. 0.5, pty -. pixel_h) in
          Some (state, state')
        else
          let state' = (px + 1, py, ptx +. pixel_w, pty) in
          Some (state, state'))
      (0, size.yres - 1, pixel_w *. 0.5, size.ylen -. (pixel_h *. 0.5))

  let%expect_test "" =
    print_s [%message (pixel_seq @@ create ~xres:5 ~yres:3 () : (int * int * float * float) Seq.t)];
    [%expect
      {|
      ("pixel_seq @@ (create ~xres:5 ~yres:3 ())"
       ((0 2 0.5 2.5) (1 2 1.5 2.5) (2 2 2.5 2.5) (3 2 3.5 2.5) (4 2 4.5 2.5)
        (0 1 0.5 1.5) (1 1 1.5 1.5) (2 1 2.5 1.5) (3 1 3.5 1.5) (4 1 4.5 1.5)
        (0 0 0.5 0.5) (1 0 1.5 0.5) (2 0 2.5 0.5) (3 0 3.5 0.5) (4 0 4.5 0.5))) |}]

  let%expect_test "" =
    for _ = 0 to 10 do
      let xres = 1 + Random.int 10 and yres = 1 + Random.int 10 in
      [%test_result: int] (Seq.length @@ pixel_seq @@ create ~xres ~yres ()) ~expect:(xres * yres)
    done
end

module T = struct
  include Hash_cached.Make (Bitarray)
end

include T
include Comparator.Make (T)

let pixels = value

let init (size : Size.t) ~f =
  let len = size.xres * size.yres in
  Bitarray.init_fold len ~init:(Size.pixel_seq size) ~f:(fun pixels idx ->
      let (_, _, ptx, pty), pixels =
        match pixels () with
        | Seq.Cons (x, xs) -> (x, xs)
        | Nil -> raise_s [%message (len : int) (size : Size.t) (idx : int)]
      in
      let ret = f idx ptx pty in
      (pixels, ret))
  |> create

let to_iter (size : Size.t) s f =
  let (_ : _ Seq.t * int) =
    Bitarray.fold (pixels s)
      ~init:(Size.pixel_seq size, 0)
      ~f:(fun (pix, idx) v ->
        let p, ps =
          match pix () with Seq.Cons (p, ps) -> (p, ps) | Nil -> raise_s [%message (size : Size.t) (idx : int)]
        in
        f (p, v);
        (ps, idx + 1))
  in
  ()

let pp fmt ((size : Size.t), x) =
  Bitarray.iteri (pixels x) ~f:(fun i b ->
      if b then Fmt.pf fmt "█" else Fmt.pf fmt ".";
      if i mod size.xres = size.xres - 1 then Fmt.pf fmt "\n")

let get x = Bitarray.get (pixels x)

let hamming c c' = Bitarray.hamming_distance (pixels c) (pixels c')

let jaccard c c' =
  let h = hamming c c' in
  let l = Bitarray.length (pixels c) in
  Float.(of_int h / of_int l)
