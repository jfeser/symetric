open Cad_ext

module type Pred_intf =
  Abstract_synth.Domain_pred_intf
    with type concrete := Value.t
     and type op := Op.t
     and type ctx := Value.Ctx.t

let abs_value size =
  let module V = struct
    type t = Pixel_set of int * int * bool | Geq of int | Lt of int
    [@@deriving compare, hash, sexp]

    let pp fmt = function
      | Pixel_set (x, y, b) -> Fmt.pf fmt "(%d, %d, %b)" x y b
      | Geq x -> Fmt.pf fmt "(%d <=)" x
      | Lt x -> Fmt.pf fmt "(< %d)" x

    let implies _ = failwith ""

    let lift : Value.t -> t Iter.t = function
      | Int _ | Rep_count _ | Error -> Iter.empty
      | Scene s ->
          Scene2d.to_iter size s |> Iter.map (fun ((x, y), v) -> Pixel_set (x, y, v))

    type arg = [ `Concrete of Value.t | `Preds of t list ]
    type ret = [ `False | `Concrete of Value.t | `Pred of t ]

    let all_ints : [> `Concrete of Value.t ] -> _ = function
      | `Concrete (Int x) -> Iter.singleton x
      | `True -> Iter.(0 -- 30)
      | _ -> assert false

    let transfer_circle ctx x y r =
      let ret = ref None in
      all_ints x (fun x ->
          all_ints y (fun y ->
              all_ints r (fun r ->
                  let v = Value.eval ctx Circle [ Int x; Int y; Int r ] in
                  match !ret with
                  | Some r -> ret := Some (Value.eval ctx Inter [ r; v ])
                  | None -> ret := Some v)));
      Iter.to_list @@ Iter.map (fun p -> `Pred p) @@ lift @@ Option.value_exn !ret

    let transfer_rect ctx lx ly hx hy =
      let min = function `Concrete (Value.Int x) -> Value.Int x | _ -> assert false in
      let max = function `Concrete (Value.Int x) -> Value.Int x | _ -> assert false in
      Iter.to_list
      @@ Iter.map (fun p -> `Pred p)
      @@ lift
      @@ Value.eval ctx Rect [ max lx; max ly; min hx; min hy ]

    let transfer_union ctx l r =
      match l, r with
      | `Concrete l, `Concrete r ->

    let transfer ctx (op : Cad_ext.Op.t) (args : arg list) : ret list =
      let all_concrete_m =
        List.map args ~f:(function `Concrete c -> Some c | _ -> None) |> Option.all
      in
      match all_concrete_m with
      | Some args -> [ `Concrete (Value.eval ctx op args) ]
      | None -> (
          match (op, args) with
          | Union, [ `Pred (Pixel_set (x, y, b)); `Pred (Pixel_set (x', y', b')) ]
            when x = x' && y = y' ->
              [ `Pred (Pixel_set (x, y, b || b')) ]
          | ( Union,
              ( [ `Pred (Pixel_set (x, y, b)); `Concrete (Scene s) ]
              | [ `Concrete (Scene s); `Pred (Pixel_set (x, y, b)) ] ) ) ->
              [ `Pred (Pixel_set (x, y, Scene2d.(get s @@ Dim.offset size x y) || b)) ]
          | Inter, [ `Pred (Pixel_set (x, y, b)); `Pred (Pixel_set (x', y', b')) ]
            when x = x' && y = y' ->
              [ `Pred (Pixel_set (x, y, b && b')) ]
          | ( Inter,
              ( [ `Pred (Pixel_set (x, y, b)); `Concrete (Scene s) ]
              | [ `Concrete (Scene s); `Pred (Pixel_set (x, y, b)) ] ) ) ->
              [ `Pred (Pixel_set (x, y, Scene2d.(get s @@ Dim.offset size x y) && b)) ]
          | Circle, [ x; y; r ] -> transfer_circle ctx x y r
          | Rect, [ lx; ly; hx; hy ] -> transfer_rect ctx lx ly hx hy
          | _ -> [])

    let cost = function `Concrete _ -> 30 | `Pred _ -> 1

    let eval p (v : Value.t) =
      match (p, v) with
      | Pixel_set (x, y, b), Scene s ->
          Bool.( = ) Scene2d.(get s @@ Dim.offset size x y) b
      | _ -> assert false
  end in
  (module V : Pred_intf)

let synth size target ops =
  let module Abs_value = (val abs_value size) in
  let module Synth = Abstract_synth.Make (Cad_ext) (Abs_value) in
  let (_ : _ Queue.t) = Synth.synth (Cad_ext.Value.Ctx.create size) target ops in
  ()

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve CAD problems with metric synthesis."
    [%map_open
      let dim = Scene2d.Dim.param in
      fun () ->
        let ectx = Value.Ctx.create dim in
        let target_prog = Cad_ext.parse @@ Sexp.input_sexp In_channel.stdin in
        let target_value = Program.eval (Value.eval ectx) target_prog in
        let ops = Cad_ext.Op.default_operators ~xres:dim.xres ~yres:dim.yres in
        synth dim target_value ops]
