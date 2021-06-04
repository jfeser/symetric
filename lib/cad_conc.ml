include Cad_conc0
module P = Dumb_params

let spec = P.Spec.create ()

let eval_calls = P.Spec.add spec @@ P.Param.float_ref ~name:"eval-calls" ()

let raw_eval_calls =
  P.Spec.add spec @@ P.Param.float_ref ~name:"raw-eval-calls" ()

let idx b v =
  let x = Float.iround_down_exn v.Vector2.x and y = Float.iround_down_exn v.y in
  let stride = ylen b in
  (x * stride) + y

let iidx b x y =
  let stride = ylen b in
  (x * stride) + y

let pt' ~ylen:stride idx =
  Vector2.
    {
      x = (Float.of_int @@ (idx / stride)) +. 0.5;
      y = (Float.of_int @@ (idx mod stride)) +. 0.5;
    }

let pt b idx = pt' ~ylen:(ylen b) idx

let getp b v = Bitarray.get (pixels b) @@ idx b v

let geti b = Bitarray.get (pixels b)

let replicate_is_set repl scene pt =
  let trans = Vector2.O.(-repl.Cad_op.v) in
  let rec loop count pt =
    if count <= 0 then false
    else
      Float.O.(
        pt.Vector2.x >= 0.0
        && pt.x < of_int (xlen scene)
        && pt.y >= 0.0
        && pt.y < of_int (ylen scene))
      && getp scene pt
      || loop (count - 1) Vector2.O.(pt + trans)
  in
  loop repl.count pt

let init params ~f =
  let bench = P.get params Cad_params.bench in
  let xlen = bench.Cad_bench.input.xmax and ylen = bench.Cad_bench.input.ymax in
  create ~xlen ~ylen
  @@ Bitarray.init (xlen * ylen) ~f:(fun i -> f (pt' ~ylen i) i)

let iinit params ~f =
  let bench = P.get params Cad_params.bench in
  let xlen = bench.Cad_bench.input.xmax and ylen = bench.Cad_bench.input.ymax in
  create ~xlen ~ylen
  @@ Bitarray.init (xlen * ylen) ~f:(fun i -> f (i / ylen) (i mod ylen))

let hamming c c' =
  Bitarray.hamming_weight @@ Bitarray.xor (pixels c) (pixels c')

let jaccard c c' =
  let h = hamming c c' in
  let l = Bitarray.length (pixels c) in
  Float.(of_int h / of_int l)

let edges c =
  let to_int x = if x then 1 else 0 in
  let above i =
    let i' = i - ylen c in
    if i' >= 0 then to_int @@ Bitarray.get (pixels c) i' else 0
  and below i =
    let i' = i + ylen c in
    if i' < Bitarray.length (pixels c) then to_int @@ Bitarray.get (pixels c) i'
    else 0
  and left i =
    if i mod ylen c = 0 then 0 else to_int @@ Bitarray.get (pixels c) (i - 1)
  and right i =
    if (i + 1) mod ylen c = 0 then 0
    else to_int @@ Bitarray.get (pixels c) (i + 1)
  in
  let pixels =
    Bitarray.init
      (xlen c * ylen c)
      ~f:(fun i ->
        Bitarray.get (pixels c) i && above i + below i + left i + right i < 4)
  in
  copy c ~pixels

let fincr r = if Float.is_nan !r then r := 1.0 else r := !r +. 1.0

exception Eval_error of Cad_op.t

let eval_unmemoized params op args =
  fincr (Params.get params eval_calls);
  match (Cad_op.value op, args) with
  | Cad_op.Inter, [ s; s' ] ->
      copy s ~pixels:(Bitarray.and_ (pixels s) (pixels s'))
  | Union, [ s; s' ] -> copy s ~pixels:(Bitarray.or_ (pixels s) (pixels s'))
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

let eval =
  let module Key = struct
    module T = struct
      type nonrec t = Cad_op.t * t list [@@deriving compare, hash, sexp]
    end

    include T
    include Comparable.Make (T)
  end in
  let tbl = Hashtbl.create (module Key) in
  let find_or_eval params op args =
    fincr (Params.get params raw_eval_calls);
    match Hashtbl.find tbl (op, args) with
    | Some v -> v
    | None ->
        let v = eval_unmemoized params op args in
        Hashtbl.set tbl ~key:(op, args) ~data:v;
        v
  in
  find_or_eval

let pprint fmt c =
  for y = ylen c - 1 downto 0 do
    for x = 0 to xlen c - 1 do
      if getp c { x = Float.of_int x; y = Float.of_int y } then Fmt.pf fmt "█"
      else Fmt.pf fmt "."
    done;
    Fmt.pf fmt "\n"
  done

let dummy = create ~xlen:(-1) ~ylen:(-1) @@ Bitarray.init 0 ~f:(fun _ -> false)

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
        P (eval_calls, ref Float.nan);
        P (raw_eval_calls, ref Float.nan);
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
