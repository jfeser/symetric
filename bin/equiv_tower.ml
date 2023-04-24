open Core
open Symetric
open Tower

exception SexpError
exception SyntaxError

let load_sexp path =
  try Sexp.load_sexp ~strict:false path with Failure _ -> raise SexpError

let parse sexp = try parse sexp with _ -> raise SyntaxError

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:{|
Check observational equivalence of two towers programs.
|}
    [%map_open
      let p1 = anon ("p1" %: string) and p2 = anon ("p2" %: string) in
      fun () ->
        let ret =
          try
            let ctx = Value.Ctx.create () in
            let p1 = parse @@ load_sexp p1 in
            let p2 = parse @@ load_sexp p2 in
            let matches =
              match
                (Program.eval (Value.eval ctx) p1, Program.eval (Value.eval ctx) p2)
              with
              | Trans t, Trans t' ->
                  Value.State.equal (List.hd_exn t.summary) (List.hd_exn t'.summary)
              | _ -> assert false
            in
            if matches then 0 else 1
          with SyntaxError | SexpError -> 2
        in
        exit ret]
  |> Command_unix.run
