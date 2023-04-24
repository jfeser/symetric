open Core
open Symetric
open Tower

let () =
  let ctx = Value.Ctx.create () in
  match Program.eval (Value.eval ctx) @@ parse @@ Sexp.input_sexp In_channel.stdin with
  | Trans t ->
      List.iter (List.hd_exn t.summary).blocks ~f:(fun b ->
          Fmt.pr "%d %d %c\n" b.x b.y (if b.kind = 0 then 'h' else 'v'))
  | _ -> ()
