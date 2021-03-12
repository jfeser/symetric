open Cad_params
open Params
module Boxes = Disj.Make (Box)

module T = struct
  type t = { upper : Boxes.t; lower : Boxes.t } [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

let eval_circle params (c : Cad_op.circle) =
  if params.lparams.concrete then
    let lower =
      Cad_conc.eval params (Circle c) []
      |> Map.to_alist
      |> List.filter ~f:(fun (_, is_in) -> is_in)
      |> List.map ~f:(fun ((p : Vector2.t), _) ->
             Box.create_closed ~xmin:p.x ~xmax:p.x ~ymin:p.y ~ymax:p.y)
      |> Boxes.of_list
    in
    { lower; upper = Boxes.top }
  else
    let upper =
      Box.create_closed
        ~xmin:Float.(c.center.x - c.radius |> round_down)
        ~xmax:Float.(c.center.x + c.radius |> round_up)
        ~ymin:Float.(c.center.y - c.radius |> round_down)
        ~ymax:Float.(c.center.y + c.radius |> round_up)
      |> Boxes.lift
    in
    { upper; lower = Boxes.bot }

let eval_rect params (r : Cad_op.rect) =
  if params.lparams.concrete then
    let lower =
      Cad_conc.eval params (Rect r) []
      |> Map.to_alist
      |> List.filter ~f:(fun (_, is_in) -> is_in)
      |> List.map ~f:(fun ((p : Vector2.t), _) ->
             Box.create_closed ~xmin:p.x ~xmax:p.x ~ymin:p.y ~ymax:p.y)
      |> Boxes.of_list
    in
    { lower; upper = Boxes.top }
  else
    let boxes =
      Box.create_closed
        ~xmin:Float.(round_down r.lo_left.x)
        ~xmax:Float.(round_up r.hi_right.x)
        ~ymin:Float.(round_down r.lo_left.y)
        ~ymax:Float.(round_up r.hi_right.y)
      |> Boxes.lift
    in
    { upper = boxes; lower = boxes }

let eval_union { upper = u; lower = l } { upper = u'; lower = l' } =
  {
    upper = Boxes.lub u u';
    lower = Boxes.to_list l @ Boxes.to_list l' |> Boxes.of_list;
  }

let eval_inter { upper = u; lower = l } { upper = u'; lower = l' } =
  { upper = Boxes.glb u u'; lower = Boxes.glb l l' }

let eval params op args =
  match (op, args) with
  | Cad_op.Circle c, [] -> eval_circle params c
  | Rect r, [] -> eval_rect params r
  | Union, [ a; b ] -> eval_union a b
  | Inter, [ a; b ] -> eval_inter a b
  | _ ->
      raise_s
      @@ [%message "unexpected arguments" (op : Cad_op.t) (args : t list)]

module Memo_key = struct
  module T = struct
    type nonrec t = Cad_op.t * t list [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)
end

let eval_memo_table = Hashtbl.create (module Memo_key)

let eval params op args =
  match Hashtbl.find eval_memo_table (op, args) with
  | Some v -> v
  | None ->
      let v = eval params op args in
      Hashtbl.set eval_memo_table ~key:(op, args) ~data:v;
      v

let roots _ = failwith "roots"

let to_symb _ = failwith "to_symb"

let is_subset a ~of_:a' =
  Boxes.is_subset a.upper ~of_:a'.upper
  && Boxes.is_superset a.lower ~of_:a'.lower

let contains a c =
  Map.for_alli c ~f:(fun ~key:v ~data:is_in ->
      if is_in then Boxes.contains a.upper v else not (Boxes.contains a.lower v))

let implies a p =
  let open Ternary in
  if Boxes.to_list a.lower |> List.exists ~f:(fun b -> Box.contains b p) then
    True
  else if
    Boxes.to_list a.upper |> List.for_all ~f:(fun b -> not (Box.contains b p))
  then False
  else Maybe

let top _ Cad_type.Scene = { upper = Boxes.top; lower = Boxes.bot }

let graphviz_pp _ fmt { lower; upper } =
  let pp_boxes =
    Fmt.using Boxes.to_list @@ Fmt.list ~sep:(Fmt.any "<br/>") Box.graphviz_pp
  in
  Fmt.pf fmt "@[<h>%a<br/><br/>%a@]" pp_boxes upper pp_boxes lower
