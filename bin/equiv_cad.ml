open Core
open Symetric
open Cad_ext

exception SexpError
exception SyntaxError

let load_sexp path =
  try Sexp.load_sexp ~strict:false path with Failure _ -> raise SexpError

let parse sexp = try parse sexp with _ -> raise SyntaxError

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      {|
Check observational equivalence of two CSG programs.

Returns:
 - 0 if the programs are equivalent
 - 1 if they differ
 - 2 if one of the programs is not a valid S-expression
 - 3 if one of the programs is not a valid CSG program
|}
    [%map_open
      let scene_width =
        flag "-scene-width" (optional_with_default 16 int) ~doc:" scene width in pixels"
      and scene_height =
        flag "-scene-height" (optional_with_default 16 int) ~doc:" scene height in pixels"
      and scaling =
        flag "-scaling" (optional_with_default 1 int) ~doc:" scene scaling factor"
      and p1 = anon ("p1" %: string)
      and p2 = anon ("p2" %: string) in
      fun () ->
        let ret =
          try
            let dim =
              Scene2d.Dim.create ~xres:scene_width ~yres:scene_height ~scaling ()
            in
            let ectx = Value.Ctx.create dim in
            let p1 = parse @@ load_sexp p1 in
            let p2 = parse @@ load_sexp p2 in
            Cad_ext.error_on_trivial := false;
            let v1 = Program.eval (Value.eval ectx) p1 in
            let v2 = Program.eval (Value.eval ectx) p2 in
            if Value.equal v1 v2 then 0 else 1
          with
          | SexpError -> 2
          | SyntaxError -> 3
        in
        exit ret]
  |> Command_unix.run
