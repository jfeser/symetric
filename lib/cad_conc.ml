include Cad_conc0

let idx b v =
  let x = Float.iround_down_exn v.Vector2.x and y = Float.iround_down_exn v.y in
  let stride = b.ylen in
  (x * stride) + y

let pt' ~ylen:stride idx =
  Vector2.
    {
      x = (Float.of_int @@ (idx / stride)) +. 0.5;
      y = (Float.of_int @@ (idx mod stride)) +. 0.5;
    }

let pt b idx = pt' ~ylen:b.ylen idx

let getp b v = Bitarray.get b.pixels @@ idx b v

let geti b = Bitarray.get b.pixels

let replicate_is_set repl scene pt =
  let trans = Vector2.O.(-repl.Cad_op.v) in
  let rec loop count pt =
    if count <= 0 then false
    else
      Float.O.(
        pt.Vector2.x >= 0.0
        && pt.x < of_int scene.xlen
        && pt.y >= 0.0
        && pt.y < of_int scene.ylen)
      && getp scene pt
      || loop (count - 1) Vector2.O.(pt + trans)
  in
  loop repl.count pt

let init params ~f =
  let xlen = params.Params.bench.Cad_bench.input.xmax
  and ylen = params.Params.bench.Cad_bench.input.ymax in
  {
    xlen;
    ylen;
    pixels = Bitarray.init (xlen * ylen) ~f:(fun i -> f (pt' ~ylen i) i);
  }

let hamming c c' = Bitarray.hamming_weight (Bitarray.xor c.pixels c'.pixels)

let jaccard c c' =
  let h = hamming c c' in
  let l = Bitarray.length c.pixels in
  Float.(of_int h / of_int l)

let eval params op args =
  try
    match (op, args) with
    | Cad_op.Inter, [ s; s' ] ->
        { s with pixels = Bitarray.and_ s.pixels s'.pixels }
    | Union, [ s; s' ] -> { s with pixels = Bitarray.or_ s.pixels s'.pixels }
    | Circle c, [] ->
        init params ~f:(fun pt _ ->
            Float.(Vector2.(l2_dist c.center pt) <= c.radius))
    | Rect r, [] ->
        init params ~f:(fun k _ ->
            Float.(
              r.lo_left.x <= k.x && r.lo_left.y <= k.y && r.hi_right.x >= k.x
              && r.hi_right.y >= k.y))
    | Replicate repl, [ s ] ->
        init params ~f:(fun pt _ -> replicate_is_set repl s pt)
    | _ -> raise_s [%message "Unexpected eval" (op : Cad_op.t)]
  with e ->
    raise Info.(to_exn @@ tag_s ~tag:[%message (op : Cad_op.t)] @@ of_exn e)

let pprint fmt c =
  for y = c.ylen - 1 downto 0 do
    for x = 0 to c.xlen - 1 do
      if getp c { x = Float.of_int x; y = Float.of_int y } then Fmt.pf fmt "█"
      else Fmt.pf fmt "."
    done;
    Fmt.pf fmt "\n"
  done

let dummy =
  { xlen = -1; ylen = -1; pixels = Bitarray.init 0 ~f:(fun _ -> false) }

let dummy_params ~xlen ~ylen =
  Params.create
    Cad_bench.
      {
        ops = [];
        input = { xmax = xlen; ymax = ylen };
        output = dummy;
        solution = None;
        filename = None;
      }
    Cad_params.{ concrete = false }

module P = struct
  type t = Cad_op.t Program.t [@@deriving compare, hash, sexp]
end

let hashable = Hashtbl.Hashable.of_key (module P)

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
