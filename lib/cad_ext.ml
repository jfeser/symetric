module Type = struct
  type t = Int | Rep_count | Scene | Error [@@deriving compare, hash, sexp]

  let output = Scene
end

module Op = struct
  type t = Union | Inter | Circle | Rect | Repl | Int of int | Rep_count of int [@@deriving compare, hash, sexp]

  let cost _ = 1

  let ret_type : _ -> Type.t = function
    | Union | Inter | Circle | Rect | Repl -> Scene
    | Int _ -> Int
    | Rep_count _ -> Rep_count

  let args_type : _ -> Type.t list = function
    | Circle -> [ Int; Int; Int ]
    | Rect -> [ Int; Int; Int; Int ]
    | Repl -> [ Scene; Int; Int; Rep_count ]
    | Union | Inter -> [ Scene; Scene ]
    | Int _ -> []
    | Rep_count _ -> []

  let arity op = List.length @@ args_type op

  let is_commutative = function Union | Inter -> true | _ -> false

  let pp fmt x = Sexp.pp_hum fmt @@ [%sexp_of: t] x

  open Program.T

  let int x = Apply (Int x, [])

  let rc x = Apply (Rep_count x, [])

  let circle x y r = Apply (Circle, [ int x; int y; int r ])

  let rect lx ly rx ry = Apply (Rect, [ int lx; int ly; int rx; int ry ])

  let empty = circle 0 0 0

  let union x y = Apply (Union, [ x; y ])

  let inter x y = Apply (Inter, [ x; y ])

  let repl dx dy c p = Apply (Repl, [ p; int dx; int dy; rc c ])
end

module Value = struct
  type t = Int of int | Rep_count of int | Scene of Scene.t | Error [@@deriving compare, hash, sexp]

  module Ctx = struct
    type t = { size : Scene.Size.t }

    let create size = { size }
  end

  let pp (ctx : Ctx.t) fmt = function
    | Scene s -> Scene.pp fmt (ctx.size, s)
    | Int x -> Fmt.pf fmt "%d" x
    | Rep_count x -> Fmt.pf fmt "%d" x
    | Error -> Fmt.pf fmt "err"

  let is_error = function Error -> true | _ -> false

  let replicate_is_set (size : Scene.Size.t) scene dx dy count x y =
    let rec loop count x y =
      if count <= 0 then false
      else
        ((x >= 0 && x < size.xres && y >= 0 && y < size.yres) && Scene.get scene (Scene.Size.offset size x y))
        || loop (count - 1) (x - dx) (y - dy)
    in
    loop count x y

  let eval_unmemoized (ctx : Ctx.t) (op : Op.t) args =
    let module S = Scene in
    let size = ctx.size in
    match (op, args) with
    | Int x, [] -> Int x
    | Rep_count x, [] -> Rep_count x
    | Circle, [ Int center_x; Int center_y; Int radius ] when radius = 0 -> Error
    | Circle, [ Int center_x; Int center_y; Int radius ] ->
        let s =
          S.init size ~f:(fun _ x y ->
              ((x - center_x) * (x - center_x)) + ((y - center_y) * (y - center_y)) <= radius * radius)
        in
        Scene s
    | Rect, [ Int lo_left_x; Int lo_left_y; Int hi_right_x; Int hi_right_y ]
      when lo_left_x >= hi_right_x || lo_left_y >= hi_right_y ->
        Error
    | Rect, [ Int lo_left_x; Int lo_left_y; Int hi_right_x; Int hi_right_y ] ->
        Scene (S.init size ~f:(fun _ x y -> lo_left_x <= x && lo_left_y <= y && hi_right_x >= x && hi_right_y >= y))
    | (Inter | Union), ([ Error; _ ] | [ _; Error ]) -> Error
    | Inter, [ Scene s; Scene s' ] ->
        let s'' = S.create (Bitarray.and_ (S.pixels s) (S.pixels s')) in
        if S.equal s s'' || S.equal s' s'' then Error else Scene s''
    | Union, [ Scene s; Scene s' ] ->
        let s'' = S.create (Bitarray.or_ (S.pixels s) (S.pixels s')) in
        if S.equal s s'' || S.equal s' s'' then Error else Scene s''
    | Repl, [ Error; Int _; Int _; Rep_count _ ] -> Error
    | Repl, [ Scene _; Int dx; Int dy; Rep_count c ] when (dx = 0 && dy = 0) || c <= 1 -> Error
    | Repl, [ Scene s; Int dx; Int dy; Rep_count c ] ->
        let s' = S.init size ~f:(fun _ -> replicate_is_set size s dx dy c) in
        if S.equal s s' then Error else Scene s'
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
