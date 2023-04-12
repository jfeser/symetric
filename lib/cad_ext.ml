let memoize = false
let error_on_trivial = ref true

module Type = struct
  type t = Int | Rep_count | Scene | Error [@@deriving compare, equal, hash, sexp]

  let default = Int
  let output = Scene
  let pp fmt x = Sexp.pp fmt @@ [%sexp_of: t] x
end

module Op = struct
  type t = Union | Inter | Circle | Rect | Repl | Sub | Int of int | Rep_count of int
  [@@deriving compare, equal, hash, sexp, yojson]

  let default = Union
  let cost _ = 1

  let ret_type : _ -> Type.t = function
    | Union | Inter | Circle | Rect | Repl | Sub -> Scene
    | Int _ -> Int
    | Rep_count _ -> Rep_count

  let args_type : _ -> Type.t list = function
    | Circle -> [ Int; Int; Int ]
    | Rect -> [ Int; Int; Int; Int ]
    | Repl -> [ Scene; Int; Int; Rep_count ]
    | Union | Inter | Sub -> [ Scene; Scene ]
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
  let union x y = Apply (Union, [ x; y ])
  let inter x y = Apply (Inter, [ x; y ])
  let repl dx dy c p = Apply (Repl, [ p; int dx; int dy; rc c ])

  let default_operators ~xres ~yres =
    [ Union; Circle; Rect; Repl; Sub ]
    @ (List.range 0 (max xres yres) |> List.map ~f:(fun i -> Int i))
    @ (List.range 2 5 |> List.map ~f:(fun i -> Rep_count i))
end

module Value = struct
  type t = Int of int | Rep_count of int | Scene of Scene2d.t | Error
  [@@deriving compare, equal, hash, sexp]

  let default = Error
  let[@inline] is_scene = function Scene _ -> true | _ -> false
  let[@inline] is_error = function Error -> true | _ -> false

  module Ctx = struct
    type t = { size : Scene2d.Dim.t }

    let create size = { size }
  end

  let pp fmt = function
    | Scene s -> Scene2d.pp fmt s
    | Int x -> Fmt.pf fmt "%d" x
    | Rep_count x -> Fmt.pf fmt "%d" x
    | Error -> Fmt.pf fmt "err"

  let eval_unmemoized (ctx : Ctx.t) (op : Op.t) args =
    let module S = Scene2d in
    let size = ctx.size in
    match (op, args) with
    | Int x, [] -> Int x
    | Rep_count x, [] -> Rep_count x
    | Circle, [ Int x; Int y; Int radius ]
      when !error_on_trivial
           && (x - radius < 0
              || x + radius >= S.Dim.xres size
              || y - radius < 0
              || y + radius >= S.Dim.yres size
              || radius = 0) ->
        Error
    | Circle, [ Int center_x; Int center_y; Int radius ] ->
        Scene (S.circle size center_x center_y radius)
    | Rect, [ Int lo_left_x; Int lo_left_y; Int hi_right_x; Int hi_right_y ]
      when !error_on_trivial && (lo_left_x >= hi_right_x || lo_left_y >= hi_right_y) ->
        Error
    | Rect, [ Int lo_left_x; Int lo_left_y; Int hi_right_x; Int hi_right_y ] ->
        Scene (S.rect size lo_left_x lo_left_y hi_right_x hi_right_y)
    | (Inter | Union | Sub), ([ Error; _ ] | [ _; Error ]) -> Error
    | Inter, [ Scene s; Scene s' ] -> Scene (S.inter s s')
    | Union, [ Scene s; Scene s' ] -> Scene (S.union s s')
    | Sub, [ Scene s; Scene s' ] -> Scene (S.sub s s')
    | Repl, [ Error; Int _; Int _; Rep_count _ ] -> Error
    | Repl, [ Scene _; Int dx; Int dy; Rep_count c ]
      when !error_on_trivial && ((dx = 0 && dy = 0) || c <= 1) ->
        Error
    | Repl, [ Scene s; Int dx; Int dy; Rep_count ct ] -> Scene (S.repeat s dx dy ct)
    | _ -> raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)]

  let mk_eval_memoized () =
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

  let eval = if memoize then mk_eval_memoized () else eval_unmemoized

  let distance v v' =
    match (v, v') with
    | Scene x, Scene x' -> Scene2d.jaccard_pixels x x'
    | v, v' -> if [%compare.equal: t] v v' then 0.0 else Float.infinity
end

let rec parse =
  let open Program.T in
  let open Op in
  let parse_int = function
    | Sexp.Atom x -> Int.of_string x
    | Sexp.List [ Atom x ] -> Int.of_string x
    | s -> raise_s [%message "expected int" (s : Sexp.t)]
  in
  function
  | Sexp.List [ Atom op; e; e' ] ->
      let op =
        match String.lowercase op with
        | "+" | "union" -> Union
        | "&" | "inter" -> Inter
        | "-" | "sub" -> Sub
        | _ -> raise_s [%message "unexpected operator" op]
      in
      Apply (op, [ parse e; parse e' ])
  | Sexp.List [ Atom op; x; y; r ] when String.(lowercase op = "circle") ->
      circle (parse_int x) (parse_int y) (parse_int r)
  | Sexp.List [ Atom op; x; y; x'; y' ] when String.(lowercase op = "rect") ->
      rect (parse_int x) (parse_int y) (parse_int x') (parse_int y')
  | Sexp.List [ Atom op; e; x; y; c ] when String.(lowercase op = "repl") ->
      repl (parse_int x) (parse_int y) (parse_int c) (parse e)
  | s -> raise_s [%message "unexpected" (s : Sexp.t)]

open Sexp

let serialize_op : Op.t -> _ = function
  | Union -> Atom "union"
  | Inter -> Atom "inter"
  | Sub -> Atom "sub"
  | Circle -> Atom "circle"
  | Rect -> Atom "rect"
  | Repl -> Atom "repl"
  | Int x | Rep_count x -> Atom (sprintf "%d" x)

let rec serialize (Program.Apply (op, args)) =
  List (serialize_op op :: List.map args ~f:serialize)
