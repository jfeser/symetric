module Boxes = Disj.Make (Box)

module T = struct
  type t = Boxes.t [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

let eval_circle (c : Cad_op.circle) =
  (* (let half_side = Float.(sqrt 2.0 / 4.0 * c.radius) in
   *  Box.create
   *    ~xmin:Float.(c.center.x - half_side |> round_up)
   *    ~xmax:Float.(c.center.x + half_side |> round_up)
   *    ~ymin:Float.(c.center.y - half_side |> round_up)
   *    ~ymax:Float.(c.center.y + half_side |> round_up)
   *  |> Boxes.lift) *)
  Box.create
    ~xmin:Float.(c.center.x - c.radius |> round_down)
    ~xmax:Float.(c.center.x + c.radius |> round_up)
    ~ymin:Float.(c.center.y - c.radius |> round_down)
    ~ymax:Float.(c.center.y + c.radius |> round_up)
  |> Boxes.lift

let eval_rect (r : Cad_op.rect) =
  (* (let half_side = Float.(sqrt 2.0 / 4.0 * c.radius) in
   *  Box.create
   *    ~xmin:Float.(c.center.x - half_side |> round_up)
   *    ~xmax:Float.(c.center.x + half_side |> round_up)
   *    ~ymin:Float.(c.center.y - half_side |> round_up)
   *    ~ymax:Float.(c.center.y + half_side |> round_up)
   *  |> Boxes.lift) *)
  Box.create
    ~xmin:Float.(round_down r.lo_left.x)
    ~xmax:Float.(round_up r.hi_right.x)
    ~ymin:Float.(round_down r.lo_left.y)
    ~ymax:Float.(round_up r.hi_right.y)
  |> Boxes.lift

let eval_union = Boxes.lub

let eval_inter = Boxes.lub

let eval _ op args =
  match (op, args) with
  | Cad_op.Circle c, [] -> eval_circle c
  | Rect r, [] -> eval_rect r
  | Union, [ a; b ] -> eval_union a b
  | Inter, [ a; b ] -> eval_inter a b
  | _ ->
      raise_s
      @@ [%message "unexpected arguments" (op : Cad_op.t) (args : t list)]

let roots _ = failwith "roots"

let to_symb _ = failwith "to_symb"

let is_subset _ = failwith "is_subset"

let contains a c =
  Map.for_alli c ~f:(fun ~key:v ~data:is_in ->
      if is_in then Boxes.contains a v else true)

let implies a p =
  let open Ternary in
  if List.exists a ~f:(fun b -> Box.is_unit b && Box.contains b p) then True
  else if List.for_all a ~f:(fun b -> not (Box.contains b p)) then False
  else Maybe

let top _ Cad_type.Scene = Boxes.top

let graphviz_pp _ = failwith "graphviz_pp"
