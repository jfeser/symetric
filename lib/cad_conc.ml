module T = struct
  type t = bool Map.M(Vector2).t [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

let getp = Map.find_exn

let hamming (params : (Cad_bench.t, _) Params.t) c =
  let ct = ref 0 in
  for x = 0 to params.bench.input.xmax - 1 do
    for y = 0 to params.bench.input.ymax - 1 do
      let p =
        Vector2.{ x = Float.of_int x +. 0.5; y = Float.of_int y +. 0.5 }
      in
      ct := !ct + if Bool.(getp c p = getp params.bench.output p) then 0 else 1
    done
  done;
  !ct

let replicate_is_set (params : (Cad_bench.t, _) Params.t) repl scene pt =
  let trans = Vector2.O.(-repl.Cad_op.v) in
  let rec loop count pt =
    if count <= 0 then false
    else
      Float.O.(
        pt.Vector2.x >= 0.0
        && pt.x < of_int params.bench.input.xmax
        && pt.y >= 0.0
        && pt.y < of_int params.bench.input.ymax)
      && getp scene pt
      || loop (count - 1) Vector2.O.(pt + trans)
  in
  loop repl.count pt

let eval params op args =
  match (op, args) with
  | Cad_op.Inter, [ s; s' ] ->
      Map.merge s s' ~f:(fun ~key:_ -> function
        | `Left x | `Right x -> Some x | `Both (x, x') -> Some (x && x'))
  | Union, [ s; s' ] ->
      Map.merge s s' ~f:(fun ~key:_ -> function
        | `Left x | `Right x -> Some x | `Both (x, x') -> Some (x || x'))
  | Circle c, [] ->
      Cad_bench.points params.Params.bench.Cad_bench.input
      |> List.map ~f:(fun k ->
             let v = Float.(Vector2.(l2_dist c.center k) <= c.radius) in
             (k, v))
      |> Map.of_alist_exn (module Vector2)
  | Rect r, [] ->
      Cad_bench.points params.Params.bench.Cad_bench.input
      |> List.map ~f:(fun k ->
             let v =
               Float.(
                 r.lo_left.x <= k.x && r.lo_left.y <= k.y && r.hi_right.x >= k.x
                 && r.hi_right.y >= k.y)
             in
             (k, v))
      |> Map.of_alist_exn (module Vector2)
  | Replicate r, [ s ] ->
      Map.mapi s ~f:(fun ~key ~data:_ -> replicate_is_set params r s key)
  | _ -> raise_s [%message "Unexpected eval" (op : Cad_op.t)]

let pprint (params : (Cad_bench.t, _) Params.t) fmt c =
  for y = params.bench.input.ymax - 1 downto 0 do
    for x = 0 to params.bench.input.xmax - 1 do
      if getp c Vector2.{ x = Float.of_int x +. 0.5; y = Float.of_int y +. 0.5 }
      then Fmt.pf fmt "█"
      else Fmt.pf fmt "."
    done;
    Fmt.pf fmt "\n"
  done

let dummy_params ~xlen ~ylen =
  Params.create
    Cad_bench.
      {
        ops = [];
        input = { xmax = xlen; ymax = ylen };
        output = Map.empty (module Vector2);
        solution = None;
        filename = None;
      }
    Cad_params.{ concrete = false }

let%expect_test "" =
  let params = dummy_params ~xlen:8 ~ylen:8 in
  eval params
    (Circle { id = 0; center = { x = 3.0; y = 3.0 }; radius = 2.0 })
    []
  |> pprint params Fmt.stdout;
  [%expect
    {|
........
........
........
........
.████...
.████...
........
........ |}]

let%expect_test "" =
  let params = dummy_params ~xlen:8 ~ylen:8 in
  eval params
    (Rect
       {
         id = 0;
         lo_left = { x = 1.0; y = 1.0 };
         hi_right = { x = 4.0; y = 4.0 };
       })
    []
  |> pprint params Fmt.stdout;
  [%expect
    {|
........
........
........
........
.███....
.███....
.███....
........ |}]

let%expect_test "" =
  let params = dummy_params ~xlen:8 ~ylen:8 in
  eval params
    (Replicate { id = 0; count = 3; v = { x = 1.0; y = 1.0 } })
    [
      eval params
        (Rect
           {
             id = 1;
             lo_left = { x = 1.0; y = 1.0 };
             hi_right = { x = 4.0; y = 4.0 };
           })
        [];
    ]
  |> pprint params Fmt.stdout;
  [%expect
    {|
........
........
...███..
..████..
.█████..
.████...
.███....
........ |}]

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

let search_compare _ =
  (module struct
    include T
    include Comparator.Make (T)
  end : Lang_abs_intf.Comparable
    with type t = t)
