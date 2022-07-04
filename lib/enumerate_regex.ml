open Std
open Regex
module Synth = Baseline.Make (Regex)

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve regex problems with enumeration."
    [%map_open
      let sketch = flag "-sketch" (required string) ~doc:" regex sketch"
      and _out = flag "-out" (required string) ~doc:" output file" in
      fun () ->
        let ctx, ops = Regex_bench.load_sketch_bench sketch In_channel.stdin in
        let synth_ctx =
          Synth.Ctx.create ctx
            (Op.default_operators 15 @ ops)
            (`Pred (fun _ v -> Float.(Regex.Value.target_distance ctx v = 0.)))
        in
        let synth = new Synth.synthesizer synth_ctx in
        print_s [%message (synth#run : Op.t Program.t option)]]
