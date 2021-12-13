open Cad_ext

module type Pred_intf =
  Abstract_synth.Domain_pred_intf with type concrete := Value.t and type op := Op.t and type ctx := Value.Ctx.t

let abs_value ?(range_width = 5) size =
  (module struct
    type t = Pixel_on of int * int | Range of int [@@deriving compare, hash, sexp]

    let complete = Iter.singleton

    let lift : Value.t -> t Iter.t = function
      | Int _ -> Iter.empty (* Iter.(0 -- (range_width - 1)) |> Iter.map (fun offset -> Range (x - offset)) *)
      | Scene s ->
          Scene.to_iter size s |> Iter.filter_map (fun ((x, y), v) -> if v then Some (Pixel_on (x, y)) else None)

    type arg = [ `True | `Concrete of Value.t | `Pred of t ]
    type ret = [ `False | `Concrete of Value.t | `Pred of t ]

    let all_ints : [> `Concrete of Value.t ] -> _ = function
      | `Concrete (Int x) -> Iter.singleton x
      | `Pred (Range l) -> Iter.(l -- (l + range_width - 1))
      | `True -> Iter.(0 -- 30)
      | _ -> assert false

    let transfer_circle ctx x y r =
      let ret = ref None in
      all_ints x (fun x ->
          all_ints y (fun y ->
              all_ints r (fun r ->
                  let v = Value.eval ctx Circle [ Int x; Int y; Int r ] in
                  match !ret with Some r -> ret := Some (Value.eval ctx Inter [ r; v ]) | None -> ret := Some v)));
      Iter.to_list @@ Iter.map (fun p -> `Pred p) @@ lift @@ Option.value_exn !ret

    let transfer_rect ctx lx ly hx hy =
      let min = function `Concrete (Value.Int x) | `Pred (Range x) -> Value.Int x | _ -> assert false in
      let max = function
        | `Concrete (Value.Int x) -> Value.Int x
        | `Pred (Range x) -> Int (x + range_width - 1)
        | _ -> assert false
      in
      Iter.to_list @@ Iter.map (fun p -> `Pred p) @@ lift @@ Value.eval ctx Rect [ max lx; max ly; min hx; min hy ]

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
            when Scene.(get s @@ Size.offset size x y) ->
              [ p ]
          | Circle, [ x; y; r ] -> transfer_circle ctx x y r
          | Rect, [ lx; ly; hx; hy ] -> transfer_rect ctx lx ly hx hy
          | _ -> [])

    let cost = function `Concrete _ -> 30 | `Pred _ -> 1

    let eval p (v : Value.t) =
      match (p, v) with
      | Range l, Int x -> l <= x && x < l + range_width
      | Pixel_on (x, y), Scene s -> Scene.(get s @@ Size.offset size x y)
      | _ -> assert false
  end : Pred_intf)

let synth size target ops =
  let module Abs_value = (val abs_value size) in
  let module Synth = Abstract_synth.Make (Cad_ext) (Abs_value) in
  let (_ : _ Queue.t) = Synth.synth (Cad_ext.Value.Ctx.create size) target ops in
  ()
