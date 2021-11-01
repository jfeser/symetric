open Cad_ext

module type Pred_intf =
  Abstract_synth.Domain_pred_intf with type concrete := Value.t and type op := Op.t and type ctx := Value.Ctx.t

let abs_value size =
  (module struct
    type t = Pixel_on of int * int [@@deriving compare, hash, sexp]

    let complete = Iter.singleton

    let lift : Value.t -> t Iter.t = function
      | Int _ -> Iter.empty
      | Scene s ->
          Scene.to_iter size s |> Iter.filter_map (fun ((x, y, _, _), v) -> if v then Some (Pixel_on (x, y)) else None)

    type arg = [ `True | `Concrete of Value.t | `Pred of t ]

    type ret = [ `False | `Concrete of Value.t | `Pred of t ]

    let transfer ctx (op : Cad_ext.Op.t) (args : arg list) : ret list =
      let all_concrete_m = List.map args ~f:(function `Concrete c -> Some c | _ -> None) |> Option.all in
      match all_concrete_m with
      | Some args ->
          let v = Value.eval ctx op args in
          `Concrete v :: (Iter.to_list @@ Iter.map (fun p -> `Pred p) @@ lift v)
      | None -> (
          match (op, args) with
          | Union, [ (`Pred _ as p); (`Pred _ as p') ] -> [ p; p' ]
          | Union, ([ (`Pred _ as p); _ ] | [ _; (`Pred _ as p) ]) -> [ p ]
          | Inter, [ (`Pred (Pixel_on (x, y)) as p); `Pred (Pixel_on (x', y')) ] when x = x' && y = y' -> [ p ]
          | ( Inter,
              ( [ (`Pred (Pixel_on (x, y)) as p); `Concrete (Scene s) ]
              | [ `Concrete (Scene s); (`Pred (Pixel_on (x, y)) as p) ] ) )
            when Scene.(get s @@ Size.offset_of_pixel size x y) ->
              [ p ]
          | _ -> [])

    let cost = function `Concrete _ -> 30 | `Pred _ -> 1

    let eval (Pixel_on (x, y)) : Value.t -> _ = function
      | Int _ -> assert false
      | Scene s -> Scene.(get s @@ Size.offset_of_pixel size x y)
  end : Pred_intf)

let synth size target ops =
  let module Abs_value = (val abs_value size) in
  let module Synth = Abstract_synth.Make (Cad_ext) (Abs_value) in
  let (_ : _ Queue.t) = Synth.synth (Cad_ext.Value.Ctx.create size) target ops in
  ()
