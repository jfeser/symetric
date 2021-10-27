module Type = struct
  type t = Int | Scene [@@deriving compare, hash, sexp]

  let output = Scene
end

module Op = struct
  type t = Union | Inter | Circle | Rect | Int of int [@@deriving compare, hash, sexp]

  let arity = function Circle -> 3 | Rect -> 4 | Union | Inter -> 2 | Int _ -> 0

  let cost _ = 1

  let ret_type : _ -> Type.t = function Union | Inter | Circle | Rect -> Scene | Int _ -> Int

  let args_type : _ -> Type.t list = function
    | Circle -> [ Int; Int; Int ]
    | Rect -> [ Int; Int; Int; Int ]
    | Union | Inter -> [ Scene; Scene ]
    | Int _ -> []
end

module Value = struct
  type t = Int of int | Scene of Scene.t [@@deriving compare, hash, sexp]

  module Ctx = struct
    type t = { size : Scene.Size.t }

    let create size = { size }
  end

  let is_error _ = false

  let eval_unmemoized (ctx : Ctx.t) op args =
    let module S = Scene in
    let size = ctx.size in
    match (op, args) with
    | Op.Inter, [ Scene s; Scene s' ] -> Scene (S.create (Bitarray.and_ (S.pixels s) (S.pixels s')))
    | Union, [ Scene s; Scene s' ] -> Scene (S.create (Bitarray.or_ (S.pixels s) (S.pixels s')))
    | Circle, [ Int center_x; Int center_y; Int radius ] ->
        let open Float in
        let center : Vector2.t = { x = of_int center_x; y = of_int center_y } and radius = of_int radius in
        Scene (S.init size ~f:(fun _ x y -> Vector2.(l2_dist center { x; y }) <= radius))
    | Rect, [ Int lo_left_x; Int lo_left_y; Int hi_right_x; Int hi_right_y ] ->
        let open Float in
        let lo_left_x = of_int lo_left_x
        and lo_left_y = of_int lo_left_y
        and hi_right_x = of_int hi_right_x
        and hi_right_y = of_int hi_right_y in
        Scene (S.init size ~f:(fun _ x y -> lo_left_x <= x && lo_left_y <= y && hi_right_x >= x && hi_right_y >= y))
    | _ -> raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)]

  let eval =
    let module Key = struct
      module T = struct
        type nonrec t = Op.t * t list [@@deriving compare, hash, sexp]
      end

      include T
      include Comparable.Make (T)
    end in
    let tbl = Hashtbl.create (module Key) in
    let find_or_eval (ctx : Ctx.t) op args =
      match Hashtbl.find tbl (op, args) with
      | Some v -> v
      | None ->
          let v = eval_unmemoized ctx op args in
          Hashtbl.set tbl ~key:(op, args) ~data:v;
          v
    in
    find_or_eval
end
