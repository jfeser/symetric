open Std

let memoize = true

module Type = Cad_ext.Type

module Op = struct
  type t = Cad_ext.Op.t =
    | Union
    | Inter
    | Circle
    | Rect
    | Repl
    | Sub
    | Int of int
    | Rep_count of int
  [@@deriving compare, hash, sexp]

  let ret_type = Cad_ext.Op.ret_type
  let cost = Cad_ext.Op.cost
  let pp = Cad_ext.Op.pp

  let args_type : _ -> Type.t list = function
    | Circle -> [ Int ]
    | Rect -> [ Int; Int ]
    | Repl -> [ Scene; Int; Int; Rep_count ]
    | Union | Inter | Sub -> [ Scene; Int; Int; Scene ]
    | Int _ -> []
    | Rep_count _ -> []

  let arity op = List.length @@ args_type op
  let is_commutative _ = false

  open Program.T

  let int x = Apply (Int x, [])
  let rc x = Apply (Rep_count x, [])
  let circle r = Apply (Circle, [ int r ])
  let rect w h = Apply (Rect, [ int w; int h ])
  let union a x y b = Apply (Union, [ a; x; y; b ])
  let inter a x y b = Apply (Inter, [ a; x; y; b ])
  let repl dx dy c p = Apply (Repl, [ p; int dx; int dy; rc c ])
end

module Value = struct
  module Ctx = Cad_ext.Value.Ctx

  type t = Int of int | Rep_count of int | Scene of (Scene2d.t * int * int) | Error
  [@@deriving compare, hash, sexp]

  let pp fmt = function
    | Scene (s, w, h) -> Fmt.pf fmt "w=%d, h=%d\n%a" w h Scene2d.pp s
    | Int x -> Fmt.pf fmt "%d" x
    | Rep_count x -> Fmt.pf fmt "%d" x
    | Error -> Fmt.pf fmt "err"

  let is_error = function Error -> true | _ -> false

  let eval_unmemoized (ctx : Ctx.t) (op : Op.t) args =
    let module S = Scene2d in
    let size = ctx.size in
    let[@inline] scene s w h =
      let w = Int.min w size.xres and h = Int.min h size.yres in
      Scene (s, w, h)
    in
    match (op, args) with
    | Int x, [] -> Int x
    | Rep_count x, [] -> Rep_count x
    | Circle, [ Int radius ] when radius = 0 -> Error
    | Circle, [ Int radius ] ->
        scene (S.circle size radius radius radius) (2 * radius) (2 * radius)
    | Rect, [ Int w; Int h ] when w <= 0 || h <= 0 -> Error
    | Rect, [ Int w; Int h ] -> Scene (S.rect size 0 0 (w - 1) (h - 1), w, h)
    | (Inter | Union | Sub), ([ Error; _; _; _ ] | [ _; _; _; Error ]) -> Error
    | Inter, _ -> failwith "unimplemented"
    | Union, [ Scene (s, w, h); Int x; Int y; Scene (s', w', h') ] ->
        scene
          (S.union (S.translate s 0 (-y)) (S.translate s' x 0))
          (Int.max w (w' + x))
          (Int.max (h - y) h')
    | Sub, [ Scene (s, w, h); Int x; Int y; Scene (s', _, _) ] ->
        scene (S.sub s (S.translate s' x y)) w h
    | Repl, [ Error; Int _; Int _; Rep_count _ ] -> Error
    | Repl, [ Scene _; Int dx; Int dy; Rep_count c ] when (dx <= 0 && dy <= 0) || c <= 1
      ->
        Error
    | Repl, [ Scene (s, w, h); Int dx; Int dy; Rep_count ct ] ->
        scene (S.repeat s dx dy ct) (w + (dx * ct)) (h + (dy * ct))
    | _ -> raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)]

  let%expect_test "" =
    let open Program.T in
    Program.eval
      (eval_unmemoized (Ctx.create @@ Scene2d.Dim.create ~xres:16 ~yres:16 ()))
      (Apply (Circle, [ Apply (Int 6, []) ]))
    |> Fmt.pr "%a\n" pp;
    [%expect
      {|
      w=12, h=12
      ................
                 ................
                 ................
                 ................
                 ...███████......
                 ..█████████.....
                 .███████████....
                 .███████████....
                 .███████████....
                 .███████████....
                 .███████████....
                 .███████████....
                 .███████████....
                 ..█████████.....
                 ...███████......
                 ................ |}]

  let%expect_test "" =
    let open Program.T in
    Program.eval
      (eval_unmemoized (Ctx.create @@ Scene2d.Dim.create ~xres:16 ~yres:16 ()))
      (Apply (Rect, [ Apply (Int 2, []); Apply (Int 2, []) ]))
    |> Fmt.pr "%a\n" pp;
    [%expect
      {|
      w=2, h=2
      ................
               ................
               ................
               ................
               ................
               ................
               ................
               ................
               ................
               ................
               ................
               ................
               ................
               ................
               ██..............
               ██.............. |}]

  let%expect_test "" =
    let open Program.T in
    Program.eval
      (eval_unmemoized (Ctx.create @@ Scene2d.Dim.create ~xres:16 ~yres:16 ()))
      (Apply
         ( Union,
           [
             Apply (Circle, [ Apply (Int 4, []) ]);
             Apply (Int 1, []);
             Apply (Int (-3), []);
             Apply (Circle, [ Apply (Int 3, []) ]);
           ] ))
    |> Fmt.pr "%a\n" pp;
    [%expect
      {|
      w=8, h=11
      ................
                ................
                ................
                ................
                ................
                ..█████.........
                .███████........
                .███████........
                .███████........
                .███████........
                .███████........
                ..█████.........
                ..█████.........
                ..█████.........
                ..█████.........
                ................ |}]

  let%expect_test "" =
    let open Program.T in
    Program.eval
      (eval_unmemoized (Ctx.create @@ Scene2d.Dim.create ~xres:16 ~yres:16 ()))
      (Apply
         ( Sub,
           [
             Apply (Circle, [ Apply (Int 4, []) ]);
             Apply (Int 1, []);
             Apply (Int (-3), []);
             Apply (Circle, [ Apply (Int 3, []) ]);
           ] ))
    |> Fmt.pr "%a\n" pp;
    [%expect
      {|
      w=8, h=8
      ................
               ................
               ................
               ................
               ................
               ................
               ................
               ................
               ..█████.........
               .███████........
               .███████........
               .███████........
               .███████........
               .█.....█........
               ................
               ................ |}]

  let eval_memoized =
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

  let eval = if memoize then eval_memoized else eval_unmemoized

  let[@inline] distance v v' =
    match (v, v') with
    | Scene (x, w, h), Scene (x', w', h') ->
        let inter = Scene2d.hamming x x' in
        let union = x.dim.scaling * Int.max w w' * x.dim.scaling * Int.max h h' in
        let ret = Float.of_int inter /. Float.of_int union in
        if inter > union then (
          Fmt.pr "d = %f,\n%a\n%a\n" ret pp v pp v';
          print_s
            [%message
              (inter : int) (union : int) (w : int) (h : int) (w' : int) (h' : int)];
          assert false);

        assert (0.0 <=. ret && ret <=. 1.0);
        ret
    | v, v' -> if [%compare.equal: t] v v' then 0.0 else Float.infinity
end

let parse_int s = Op.int @@ [%of_sexp: int] s
let parse_count s = Op.rc @@ [%of_sexp: int] s

let rec parse =
  let open Program.T in
  let open Op in
  function
  | Sexp.List [ Atom "union"; e; x; y; e' ] ->
      Apply (Union, [ parse e; parse_int x; parse_int y; parse e' ])
  | Sexp.List [ Atom "inter"; e; x; y; e' ] ->
      Apply (Inter, [ parse e; parse_int x; parse_int y; parse e' ])
  | Sexp.List [ Atom "sub"; e; x; y; e' ] ->
      Apply (Sub, [ parse e; parse_int x; parse_int y; parse e' ])
  | Sexp.List [ Atom "repl"; e; x; y; c ] ->
      Apply (Repl, [ parse e; parse_int x; parse_int y; parse_count c ])
  | Sexp.List [ Atom op; r ] when String.(lowercase op = "circle") ->
      circle ([%of_sexp: int] r)
  | Sexp.List [ Atom op; x; y ] when String.(lowercase op = "rect") ->
      rect ([%of_sexp: int] x) ([%of_sexp: int] y)
  | s -> raise_s [%message "unexpected" (s : Sexp.t)]

let serialize = Cad_ext.serialize
