open Cad_ext

module type Pred_intf =
  Abstract_synth.Domain_pred_intf
    with type concrete := Value.t
     and type op := Op.t
     and type ctx := Value.Ctx.t

module I2 = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

let abs_value size =
  let module V = struct
    type t = Pixel_set of int * int * bool [@@deriving compare, equal, hash, sexp]

    let pp fmt = function Pixel_set (x, y, b) -> Fmt.pf fmt "(%d, %d, %b)" x y b

    let summarize = function
      | `Preds preds ->
          `Scene
            (List.map preds ~f:(function Pixel_set (x, y, b) -> ((x, y), b))
            |> Map.of_alist_exn (module I2))
      | `Concrete (Value.Scene s) ->
          let iteri ~f = Scene2d.to_iter size s (fun (k, v) -> f ~key:k ~data:v) in
          `Scene (Map.of_iteri_exn (module I2) ~iteri)
      | `Concrete (Value.Int x) -> `Int x
      | `Concrete Error -> `Error
      | `Concrete (Rep_count x) -> `Rep_count x

    let summarize_int x =
      match summarize x with `Int x -> x | _ -> failwith "not an int"

    let summarize_rep_ct x =
      match summarize x with `Rep_count x -> x | _ -> failwith "not a rep count"

    let summarize_scene x =
      match summarize x with
      | `Scene x -> `Scene x
      | `Error -> `Error
      | _ -> failwith "not a scene"

    let implies a b =
      match (summarize a, summarize b) with
      | `Scene a, `Scene b ->
          Map.fold_symmetric_diff a b ~data_equal:Bool.( = ) ~init:true ~f:(fun acc ->
            function _, `Left _ -> acc | _, (`Right _ | `Unequal _) -> false)
      | `Int a, `Int b -> a = b
      | `Error, `Error -> true
      | _ -> false

    let lift : Value.t -> t Iter.t = function
      | Int _ | Rep_count _ | Error -> Iter.empty
      | Scene s ->
          Scene2d.to_iter size s |> Iter.map (fun ((x, y), v) -> Pixel_set (x, y, v))

    type arg = [ `Concrete of Value.t | `Preds of t list ] [@@deriving sexp]
    type ret = [ `False | `Concrete of Value.t | `Preds of t list ] [@@deriving sexp]

    let blast_scene su =
      Map.to_alist su |> List.map ~f:(function (x, y), b -> Pixel_set (x, y, b))

    let preds x = `Preds x

    let transfer_union l r =
      match (summarize_scene l, summarize_scene r) with
      | `Scene l, `Scene r ->
          let preds =
            Map.merge l r ~f:(fun ~key:_ -> function
              | `Both (a, b) -> Some (a || b)
              | `Left true | `Right true -> Some true
              | _ -> None)
            |> blast_scene
          in
          (None, preds)
      | `Error, _ | _, `Error -> (Some Value.Error, [])

    let transfer_sub l r =
      match (summarize_scene l, summarize_scene r) with
      | `Scene l, `Scene r ->
          let preds =
            Map.merge l r ~f:(fun ~key:_ -> function
              | `Both (a, b) -> Some (a && not b)
              | `Left false | `Right true -> Some false
              | _ -> None)
            |> blast_scene
          in
          (None, preds)
      | `Error, _ | _, `Error -> (Some Value.Error, [])

    let transfer_repl s dx dy ct =
      let dx = summarize_int dx and dy = summarize_int dy and ct = summarize_rep_ct ct in

      match summarize_scene s with
      | `Scene s ->
          let preds =
            Map.to_alist s
            |> List.concat_map ~f:(fun ((x, y), b) ->
                   let rec forced_on c =
                     if c <= ct then
                       let x' = x + (dx * c) and y' = y + (dy * c) in
                       if
                         x' < 0
                         || x' >= Scene2d.Dim.scaled_xres size
                         || y' < 0
                         || y' >= Scene2d.Dim.scaled_yres size
                       then []
                       else Pixel_set (x', y', b) :: forced_on (c + 1)
                     else []
                   in
                   let rec forced_off c =
                     if c <= ct then
                       let x' = x - (dx * c) and y' = y - (dy * c) in
                       if
                         x' < 0
                         || x' >= Scene2d.Dim.scaled_xres size
                         || y' < 0
                         || y' >= Scene2d.Dim.scaled_yres size
                       then [ Pixel_set (x, y, b) ]
                       else
                         match Map.find s (x', y') with
                         | Some false -> forced_off (c + 1)
                         | Some _ | None -> []
                     else []
                   in

                   if b then forced_on 1 else forced_off 1)
          in
          (None, preds)
      | `Error -> (Some Value.Error, [])

    let transfer_circle x y r =
      let x = summarize_int x and y = summarize_int y and r = summarize_int r in
      Scene2d.circle size x y r |> Scene2d.to_iter size |> Iter.to_list
      |> Map.of_alist_exn (module I2)
      |> blast_scene

    let transfer_rect lx ly hx hy =
      let lx = summarize_int lx
      and ly = summarize_int ly
      and hx = summarize_int hx
      and hy = summarize_int hy in
      Scene2d.rect size lx ly hx hy |> Scene2d.to_iter size |> Iter.to_list
      |> Map.of_alist_exn (module I2)
      |> blast_scene

    let transfer ctx (op : Cad_ext.Op.t) (args : arg list) =
      if List.exists args ~f:(function `Preds [] -> true | _ -> false) then
        (false, None, [])
      else
        let all_concrete_m =
          List.map args ~f:(function `Concrete c -> Some c | _ -> None) |> Option.all
        in
        match all_concrete_m with
        | Some args ->
            let c = Value.eval ctx op args in
            (false, Some c, Iter.to_list @@ lift c)
        | None ->
            let concrete, preds =
              match (op, args) with
              | Union, [ a; b ] -> transfer_union a b
              | Sub, [ a; b ] -> transfer_sub a b
              | Circle, [ x; y; r ] -> (None, transfer_circle x y r)
              | Rect, [ lx; ly; hx; hy ] -> (None, transfer_rect lx ly hx hy)
              | Repl, [ s; dx; dy; ct ] -> transfer_repl s dx dy ct
              | _ -> failwith "unexpected arguments"
            in
            (false, concrete, preds)

    let transfer ctx op args =
      let ret = transfer ctx op args in
      (* print_s *)
      (*   [%message *)
      (*     (op : Op.t) (args : _ list) (\* (ret : bool * Value.t option * t list) *\)]; *)
      ret

    let cost = function `Concrete (Value.Scene _) -> 4 | `Concrete _ | `Pred _ -> 1

    let eval p (v : Value.t) =
      match (p, v) with
      | Pixel_set (x, y, b), Scene s ->
          Bool.( = ) Scene2d.(get s @@ Dim.offset size x y) b
      | Pixel_set _, _ -> failwith "not a scene"
  end in
  (module V : Pred_intf)

let synth size target ops =
  let module Abs_value = (val abs_value size) in
  let module Synth = Abstract_synth.Make (Cad_ext) (Abs_value) in
  let (_ : _ Queue.t) = Synth.synth_simple (Cad_ext.Value.Ctx.create size) target ops in
  ()

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve CAD problems with metric synthesis."
    [%map_open
      let dim = Scene2d.Dim.param
      and no_repl = flag "-no-replicate" no_arg ~doc:" disable replicate op" in
      fun () ->
        let ectx = Value.Ctx.create dim in
        let target_prog = Cad_ext.parse @@ Sexp.input_sexp In_channel.stdin in
        let target_value = Program.eval (Value.eval ectx) target_prog in
        let ops = Cad_ext.Op.default_operators ~xres:dim.xres ~yres:dim.yres in
        let ops =
          if no_repl then
            List.filter ops ~f:(function Repl | Rep_count _ -> false | _ -> true)
          else ops
        in
        synth dim target_value ops]
