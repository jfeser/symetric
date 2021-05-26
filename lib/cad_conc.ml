include Cad_conc0
module P = Dumb_params

let spec = P.Spec.create ()

let eval_calls = P.Spec.add spec @@ P.Param.float_ref ~name:"eval-calls" ()

let idx b v =
  let x = Float.iround_down_exn v.Vector2.x and y = Float.iround_down_exn v.y in
  let stride = b.ylen in
  (x * stride) + y

let iidx b x y =
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
  let bench = P.get params Cad_params.bench in
  let xlen = bench.Cad_bench.input.xmax and ylen = bench.Cad_bench.input.ymax in
  {
    xlen;
    ylen;
    pixels = Bitarray.init (xlen * ylen) ~f:(fun i -> f (pt' ~ylen i) i);
  }

let iinit params ~f =
  let bench = P.get params Cad_params.bench in
  let xlen = bench.Cad_bench.input.xmax and ylen = bench.Cad_bench.input.ymax in
  {
    xlen;
    ylen;
    pixels = Bitarray.init (xlen * ylen) ~f:(fun i -> f (i / ylen) (i mod ylen));
  }

let hamming c c' = Bitarray.hamming_weight (Bitarray.xor c.pixels c'.pixels)

let jaccard c c' =
  let h = hamming c c' in
  let l = Bitarray.length c.pixels in
  Float.(of_int h / of_int l)

let edges c =
  let to_int x = if x then 1 else 0 in
  let above i =
    let i' = i - c.ylen in
    if i' >= 0 then to_int @@ Bitarray.get c.pixels i' else 0
  and below i =
    let i' = i + c.ylen in
    if i' < Bitarray.length c.pixels then to_int @@ Bitarray.get c.pixels i'
    else 0
  and left i =
    if i mod c.ylen = 0 then 0 else to_int @@ Bitarray.get c.pixels (i - 1)
  and right i =
    if (i + 1) mod c.ylen = 0 then 0 else to_int @@ Bitarray.get c.pixels (i + 1)
  in
  {
    c with
    pixels =
      Bitarray.init (c.xlen * c.ylen) ~f:(fun i ->
          Bitarray.get c.pixels i && above i + below i + left i + right i < 4);
  }

let fincr r = if Float.is_nan !r then r := 1.0 else r := !r +. 1.0

exception Eval_error of Cad_op.t

let eval params op args =
  (* fincr (Params.get params eval_calls); *)
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
  | _ -> raise (Eval_error op)

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
  P.(
    of_alist_exn
      [
        P
          ( Cad_params.bench,
            Cad_bench.
              {
                ops = [];
                input = { xmax = xlen; ymax = ylen };
                output = dummy;
                solution = None;
                filename = None;
              } );
      ])

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
