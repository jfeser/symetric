open Core
open Staged_synth
open Tower

let () =
  let ctx = Value.Ctx.create () in
  match Program.eval (Value.eval ctx) @@ parse @@ Sexp.input_sexp In_channel.stdin with
  | Trans t -> Value.pp ctx Fmt.stdout (List.hd_exn t.summary).blocks
  | _ -> ()
