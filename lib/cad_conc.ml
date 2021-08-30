open Std
include Cad_conc0

include struct
  open Dumb_params

  let spec = Spec.create ~name:"cad-value" ()

  let eval_calls = Spec.add spec @@ Param.float_ref ~name:"eval-calls" ()

  let raw_eval_calls = Spec.add spec @@ Param.float_ref ~name:"raw-eval-calls" ()

  let embed_fn =
    Spec.add spec
    @@ Param.string ~name:"embed-file" ~init:(`Cli (Some "")) ~doc:" file containing neural network for embedding" ()
end

module Ctx = struct
  type t = {
    xlen : int;
    ylen : int;
    embedder : Torch.Module.t option;
    eval_calls : float ref;
    raw_eval_calls : float ref;
  }

  let create ?stats ?embed_file ~xlen ~ylen () =
    let stats = Option.value_lazy stats ~default:(lazy (Stats.create ())) in
    {
      xlen;
      ylen;
      embedder = Option.map embed_file ~f:Torch.Module.load;
      eval_calls = Stats.add_probe_exn stats "eval-calls";
      raw_eval_calls = Stats.add_probe_exn stats "raw-eval-calls";
    }

  let of_params params =
    let bench = Params.get params Cad_params.bench in
    let xlen = bench.Cad_bench.input.xmax and ylen = bench.Cad_bench.input.ymax in
    let embed_fn = Params.get params embed_fn in
    let embedder = Option.map ~f:Torch.Module.load @@ Option.some_if String.(embed_fn <> "") embed_fn in
    {
      xlen;
      ylen;
      embedder;
      eval_calls = Params.get params eval_calls;
      raw_eval_calls = Params.get params raw_eval_calls;
    }
end

let iidx b x y =
  let stride = ylen b in
  (x * stride) + y

let pt' ~ylen:stride idx =
  Vector2.{ x = (Float.of_int @@ (idx / stride)) +. 0.5; y = (Float.of_int @@ (idx mod stride)) +. 0.5 }

let pt b idx = pt' ~ylen:(ylen b) idx

let geti b = Bitarray.get (pixels b)

let replicate_is_set repl scene pt =
  let trans = Vector2.O.(-repl.Cad_op.v) in
  let rec loop count pt =
    if count <= 0 then false
    else
      Float.O.(pt.Vector2.x >= 0.0 && pt.x < of_int (xlen scene) && pt.y >= 0.0 && pt.y < of_int (ylen scene))
      && getp scene pt
      || loop (count - 1) Vector2.O.(pt + trans)
  in
  loop repl.count pt

let init (ctx : Ctx.t) ~f =
  let xlen = ctx.xlen and ylen = ctx.ylen in
  create ~xlen ~ylen @@ Bitarray.init (xlen * ylen) ~f:(fun i -> f (pt' ~ylen i) i)

let iinit (ctx : Ctx.t) ~f =
  let xlen = ctx.xlen and ylen = ctx.ylen in
  create ~xlen ~ylen @@ Bitarray.init (xlen * ylen) ~f:(fun i -> f (i / ylen) (i mod ylen))

let hamming c c' = Bitarray.hamming_distance (pixels c) (pixels c')

let jaccard c c' =
  let h = hamming c c' in
  let l = Bitarray.length (pixels c) in
  Float.(of_int h / of_int l)

let to_ndarray v = Bitarray.to_ndarray (pixels v)

let normalize vecs =
  let open Torch in
  let lengths = Tensor.(clamp_min ~min:(Scalar.f 1e-12) @@ norm_except_dim ~v:vecs ~pow:1 ~dim:0) in
  Tensor.(vecs / lengths)

let nn_embed model vs =
  let open Torch in
  let input =
    Tensor.to_device ~device:(Device.Cuda 0)
    @@ Tensor.stack ~dim:0
    @@ List.map vs ~f:(fun v -> Tensor.reshape ~shape:[ 1; 30; 30 ] @@ Bitarray.to_torch (pixels v))
  in
  let vecs = Module.forward model [ input ] in
  let lengths = Tensor.(clamp_min ~min:(Scalar.f 1e-12) @@ norm_except_dim ~v:vecs ~pow:2 ~dim:0) in
  let vecs = Tensor.(vecs / lengths) in
  vecs

let no_embed vs =
  let open Torch in
  normalize
  @@ Tensor.to_device ~device:(Device.Cuda 0)
  @@ Tensor.stack ~dim:0
  @@ List.map vs ~f:(fun v -> Bitarray.to_torch (pixels v))

let embed (ctx : Ctx.t) = match ctx.embedder with Some model -> nn_embed model | None -> no_embed

let dist ctx =
  let embed = embed ctx in
  fun v v' -> Torch.Tensor.(to_float0_exn @@ norm_except_dim ~dim:0 ~pow:1 ~v:(embed [ v ] - embed [ v' ]))

let edges c =
  let to_int x = if x then 1 else 0 in
  let above i =
    let i' = i - ylen c in
    if i' >= 0 then to_int @@ Bitarray.get (pixels c) i' else 0
  and below i =
    let i' = i + ylen c in
    if i' < Bitarray.length (pixels c) then to_int @@ Bitarray.get (pixels c) i' else 0
  and left i = if i mod ylen c = 0 then 0 else to_int @@ Bitarray.get (pixels c) (i - 1)
  and right i = if (i + 1) mod ylen c = 0 then 0 else to_int @@ Bitarray.get (pixels c) (i + 1) in
  let pixels =
    Bitarray.init (xlen c * ylen c) ~f:(fun i -> Bitarray.get (pixels c) i && above i + below i + left i + right i < 4)
  in
  copy c ~pixels

let fincr r = if Float.is_nan !r then r := 1.0 else r := !r +. 1.0

exception Eval_error of Cad_op.t

let eval_unmemoized (ctx : Ctx.t) op args =
  fincr ctx.eval_calls;
  match (Cad_op.value op, args) with
  | Cad_op.Inter, [ s; s' ] -> copy s ~pixels:(Bitarray.and_ (pixels s) (pixels s'))
  | Union, [ s; s' ] -> copy s ~pixels:(Bitarray.or_ (pixels s) (pixels s'))
  | Circle c, [] -> init ctx ~f:(fun pt _ -> Float.(Vector2.(l2_dist c.center pt) <= c.radius))
  | Rect r, [] ->
      init ctx ~f:(fun k _ ->
          Float.(r.lo_left.x <= k.x && r.lo_left.y <= k.y && r.hi_right.x >= k.x && r.hi_right.y >= k.y))
  | Replicate repl, [ s ] -> init ctx ~f:(fun pt _ -> replicate_is_set repl s pt)
  | _ -> raise (Eval_error op)

let eval =
  let module Key = struct
    module T = struct
      type nonrec t = Cad_op.t * t list [@@deriving compare, hash, sexp]
    end

    include T
    include Comparable.Make (T)
  end in
  let tbl = Hashtbl.create (module Key) in
  let find_or_eval (ctx : Ctx.t) op args =
    fincr ctx.raw_eval_calls;
    match Hashtbl.find tbl (op, args) with
    | Some v -> v
    | None ->
        let v = eval_unmemoized ctx op args in
        Hashtbl.set tbl ~key:(op, args) ~data:v;
        v
  in
  find_or_eval

let pprint fmt c =
  for y = ylen c - 1 downto 0 do
    for x = 0 to xlen c - 1 do
      if getp c { x = Float.of_int x; y = Float.of_int y } then Fmt.pf fmt "█" else Fmt.pf fmt "."
    done;
    Fmt.pf fmt "\n"
  done

let dummy = create ~xlen:(-1) ~ylen:(-1) @@ Bitarray.init 0 ~f:(fun _ -> false)

let dummy_params ~xlen ~ylen =
  let open Dumb_params in
  of_alist_exn
    [
      P
        ( Cad_params.bench,
          Cad_bench.{ ops = []; input = { xmax = xlen; ymax = ylen }; output = dummy; solution = None; filename = None }
        );
      P (eval_calls, ref Float.nan);
      P (raw_eval_calls, ref Float.nan);
    ]

module Prog = struct
  type t = Cad_op.t Program.t [@@deriving compare, hash, sexp]
end

let hashable = Hashtbl.Hashable.of_key (module Prog)

let table = Hash_queue.create hashable

let rec eval_program params p =
  match Hash_queue.lookup_and_move_to_front table p with
  | Some v -> v
  | None ->
      let (Program.Apply (op, args)) = p in
      let v = eval params op (List.map args ~f:(eval_program params)) in
      Hash_queue.enqueue_back_exn table p v;
      v

let roots _ = failwith "unimplemented roots"

let to_symb _ = failwith "unimplemented to_symb"

let leq _ = failwith "unimplemented is_subset"

let contains = [%compare.equal: t]

let graphviz_pp _ = failwith "unimplemented pp"

let top _ = failwith "unimplemented top"

open Owl

let to_matrix v = Mat.init (xlen v) (ylen v) (fun i -> if geti v i then 1.0 else 0.0) |> Mat.flip

let ( =. ) = Float.( = )

let ( >. ) = Float.( > )

let of_matrix m =
  let xlen, ylen = Mat.shape m and m' = Mat.flatten @@ Mat.rotate m 90 in
  create ~xlen ~ylen (Bitarray.init (xlen * ylen) ~f:(fun i -> Owl.Dense.Ndarray.Generic.get m' [| i |] >. 0.0))

let find_mask img mask =
  let img_w, img_h = Mat.shape img and mask_w, mask_h = Mat.shape mask in
  let ret_w = img_w - mask_w + 1 and ret_h = img_h - mask_h + 1 in
  let ret = Mat.zeros ret_w ret_h in
  for i = 0 to ret_w - 1 do
    for j = 0 to ret_h - 1 do
      let v = ref true in
      for i' = 0 to mask_w - 1 do
        for j' = 0 to mask_h - 1 do
          v := !v && Mat.get mask i' j' =. Mat.get img (i + i') (j + j')
        done
      done;
      Mat.set ret i j (if !v then 1.0 else 0.0)
    done
  done;
  ret

let ul_corner = Mat.of_arrays [| [| 0.0; 0.0; 0.0 |]; [| 0.0; 1.0; 1.0 |]; [| 0.0; 1.0; 1.0 |] |]

let ur_corner = Mat.of_arrays [| [| 0.0; 0.0; 0.0 |]; [| 1.0; 1.0; 0.0 |]; [| 1.0; 1.0; 0.0 |] |]

let ll_corner = Mat.of_arrays [| [| 0.0; 1.0; 1.0 |]; [| 0.0; 1.0; 1.0 |]; [| 0.0; 0.0; 0.0 |] |]

let lr_corner = Mat.of_arrays [| [| 1.0; 1.0; 0.0 |]; [| 1.0; 1.0; 0.0 |]; [| 0.0; 0.0; 0.0 |] |]

let vert_r = Mat.of_arrays [| [| 0.0; 1.0 |]; [| 0.0; 1.0 |] |]

let vert_l = Mat.of_arrays [| [| 1.0; 0.0 |]; [| 1.0; 0.0 |] |]

let hori_u = Mat.of_arrays [| [| 0.0; 0.0 |]; [| 1.0; 1.0 |] |]

let hori_l = Mat.of_arrays [| [| 1.0; 1.0 |]; [| 0.0; 0.0 |] |]

let masks = [| ul_corner; ur_corner; ll_corner; lr_corner; vert_r; vert_l; hori_u; hori_l |]

let features v =
  let img = to_matrix v in
  Array.map masks ~f:(fun m -> Owl.Dense.Ndarray.Generic.get (find_mask img m |> Mat.sum) [| 0 |])

open Cad_op

let ctx = Ctx.create ~xlen:8 ~ylen:8 ()

let%expect_test "" =
  let v =
    eval ctx
      (replicate ~id:0 ~count:3 ~v:{ x = 1.0; y = 1.0 })
      [ eval ctx (rect ~id:1 ~lo_left:{ x = 1.0; y = 1.0 } ~hi_right:{ x = 4.0; y = 4.0 }) [] ]
  in
  let img = to_matrix v and mask = ll_corner in
  let masked = find_mask img mask in
  Mat.print img;
  Mat.print mask;
  Mat.print masked;
  pprint Fmt.stdout (of_matrix masked);
  [%expect {|
       C0 C1 C2 C3 C4 C5 C6 C7
    R0  0  0  0  0  0  0  0  0
    R1  0  0  0  0  0  0  0  0
    R2  0  0  0  1  1  1  0  0
    R3  0  0  1  1  1  1  0  0
    R4  0  1  1  1  1  1  0  0
    R5  0  1  1  1  1  0  0  0
    R6  0  1  1  1  0  0  0  0
    R7  0  0  0  0  0  0  0  0


       C0 C1 C2
    R0  0  1  1
    R1  0  1  1
    R2  0  0  0

       C0 C1 C2 C3 C4 C5
    R0  0  0  0  0  0  0
    R1  0  0  0  0  0  0
    R2  0  0  0  0  0  0
    R3  0  0  0  0  0  0
    R4  0  0  0  0  0  0
    R5  1  0  0  0  0  0
    ......
    ......
    ......
    ......
    ......
    █..... |}]

let%expect_test "" =
  let v =
    eval ctx
      (replicate ~id:0 ~count:3 ~v:{ x = 1.0; y = 1.0 })
      [ eval ctx (rect ~id:1 ~lo_left:{ x = 1.0; y = 1.0 } ~hi_right:{ x = 4.0; y = 4.0 }) [] ]
  in
  print_s [%message (features v : float array)];
  [%expect {| ("features v" (0 1 1 0 2 2 2 2)) |}]
