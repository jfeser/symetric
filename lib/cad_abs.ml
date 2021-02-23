module T = struct
  type t = Pbox.t [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

let eval_circle (c : Cad_op.circle) =
  Pbox.create
    ~lo:
      (let half_side = Float.(sqrt 2.0 / 4.0 * c.radius) in
       Box.create
         ~xmin:Float.(c.center.x - half_side)
         ~xmax:Float.(c.center.x + half_side)
         ~ymin:Float.(c.center.y - half_side)
         ~ymax:Float.(c.center.y + half_side))
    ~hi:
      (Box.create
         ~xmin:Float.(c.center.x - c.radius)
         ~xmax:Float.(c.center.x + c.radius)
         ~ymin:Float.(c.center.y - c.radius)
         ~ymax:Float.(c.center.y + c.radius))

let eval_union = Pbox.lub

let eval_inter = Pbox.glb

let eval _ op args =
  match (op, args) with
  | Cad_op.Circle c, [] -> eval_circle c
  | Union, [ a; b ] -> eval_union a b
  | Inter, [ a; b ] -> eval_inter a b
  | _ ->
      raise_s
      @@ [%message "unexpected arguments" (op : Cad_op.t) (args : t list)]

let roots _ = failwith "roots"

let to_symb _ = failwith "to_symb"

let is_subset _ = failwith "is_subset"

let contains a (c, g) =
  Range.for_all { lo = 0; hi = g.Cad_bench.xmax } ~f:(fun x ->
      Range.for_all { lo = 0; hi = g.Cad_bench.ymax } ~f:(fun y ->
          let x = Float.of_int x and y = Float.of_int y in
          (not (c x y)) || Pbox.contains a (x, y)))

let top _ Cad_type.Scene = Pbox.top

let graphviz_pp _ = failwith "graphviz_pp"
