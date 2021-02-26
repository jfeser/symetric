include Disj.Make (Pbox)

let eval_circle (c : Cad_op.circle) =
  Pbox.create
    ~lo:
      (let half_side = Float.(sqrt 2.0 / 4.0 * c.radius) in
       Box.create
         ~xmin:Float.(c.center.x - half_side |> round_up)
         ~xmax:Float.(c.center.x + half_side |> round_up)
         ~ymin:Float.(c.center.y - half_side |> round_up)
         ~ymax:Float.(c.center.y + half_side |> round_up))
    ~hi:
      (Box.create
         ~xmin:Float.(c.center.x - c.radius |> round_down)
         ~xmax:Float.(c.center.x + c.radius |> round_down)
         ~ymin:Float.(c.center.y - c.radius |> round_down)
         ~ymax:Float.(c.center.y + c.radius |> round_down))
  |> lift

let eval_union = lub

let eval_inter = glb

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

let contains a c =
  Set.for_all c ~f:(fun p ->
      List.exists a ~f:(fun b ->
          Pbox.contains b (Set.singleton (module Vector2) p)))

let top _ Cad_type.Scene = [ Pbox.top ]

let graphviz_pp _ = failwith "graphviz_pp"
