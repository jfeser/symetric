open Cad_params
open Params
module Boxes = Disj.Make (Box)

module T = struct
  type t = Boxes.t [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

let eval_circle params (c : Cad_op.circle) =
  if params.lparams.concrete then
    Cad_conc.eval params (Circle c) []
    |> Map.to_alist
    |> List.filter ~f:(fun (_, is_in) -> is_in)
    |> List.map ~f:(fun ((p : Vector2.t), _) ->
           Box.create_closed ~approx:false ~xmin:p.x ~xmax:p.x ~ymin:p.y
             ~ymax:p.y)
    |> Boxes.of_list
  else
    Box.create_closed ~approx:true
      ~xmin:Float.(c.center.x - c.radius |> round_down)
      ~xmax:Float.(c.center.x + c.radius |> round_up)
      ~ymin:Float.(c.center.y - c.radius |> round_down)
      ~ymax:Float.(c.center.y + c.radius |> round_up)
    |> Boxes.lift

let eval_rect params (r : Cad_op.rect) =
  if params.lparams.concrete then
    Cad_conc.eval params (Rect r) []
    |> Map.to_alist
    |> List.filter ~f:(fun (_, is_in) -> is_in)
    |> List.map ~f:(fun ((p : Vector2.t), _) ->
           Box.create_closed ~approx:false ~xmin:p.x ~xmax:p.x ~ymin:p.y
             ~ymax:p.y)
    |> Boxes.of_list
  else
    Box.create_closed ~approx:false
      ~xmin:Float.(round_down r.lo_left.x)
      ~xmax:Float.(round_up r.hi_right.x)
      ~ymin:Float.(round_down r.lo_left.y)
      ~ymax:Float.(round_up r.hi_right.y)
    |> Boxes.lift

let eval_union = Boxes.lub

let eval_inter = Boxes.glb

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

let is_subset _ = failwith "is_subset"

let contains a c =
  Map.for_alli c ~f:(fun ~key:v ~data:is_in ->
      if is_in then Boxes.contains a v
      else
        Boxes.to_list a
        |> List.for_all ~f:(fun b -> b.Box.approx || not (Box.contains b v)))

let implies a p =
  let open Ternary in
  let a = Boxes.to_list a in
  if List.exists a ~f:(fun b -> (not b.Box.approx) && Box.contains b p) then
    True
  else if List.for_all a ~f:(fun b -> not (Box.contains b p)) then False
  else Maybe

let top _ Cad_type.Scene = Boxes.top

let graphviz_pp _ =
  Fmt.hbox @@ Fmt.using Boxes.to_list
  @@ Fmt.list ~sep:(Fmt.any "<br/>") Box.graphviz_pp
