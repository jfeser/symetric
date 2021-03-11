module T = struct
  type t = bool Map.M(Vector2).t [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

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
  | _ -> raise_s [%message "Unexpected eval" (op : Cad_op.t)]

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

let is_subset _ = failwith "unimplemented is_subset"

let contains = [%compare.equal: t]

let graphviz_pp _ = failwith "unimplemented pp"

let top _ = failwith "unimplemented top"
