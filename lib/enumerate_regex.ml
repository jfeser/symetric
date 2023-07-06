open Std

let mk_log sketch ops (ctx : Regex.Value.Ctx.t) output synth_log =
  output
    (`Assoc
      [
        ("synth", synth_log);
        ( "dsl",
          `Assoc
            [
              ("ops", [%yojson_of: Regex.Op.t list] ops);
              ("sketch", `String sketch);
              ("inputs", [%yojson_of: (string * bool) list] ctx.input);
            ] );
      ])

let synthesize ?log (synth_params : Baseline.Params.t) bench =
  let vctx, ops = bench in
  let module Dsl = struct
    include Regex

    module Value = struct
      include Value

      let eval = eval vctx
    end

    let operators = Op.default_operators 15 @ ops
  end in
  Baseline.synthesize ?log
    (module Dsl)
    synth_params
    (`Pred (fun _ v -> Float.(Regex.Value.target_distance vctx v = 0.)))

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve regex problems with enumeration."
    [%map_open
      let synth_params = Baseline.Params.param
      and sketch = flag "-sketch" (required string) ~doc:" regex sketch"
      and out = flag "-out" (optional string) ~doc:" output file" in
      fun () ->
        let ((ctx, ops) as bench) =
          Regex_bench.load_sketch_bench sketch In_channel.stdin
        in
        synthesize
          ?log:
            (Option.map out ~f:(fun out_file ->
                 mk_log sketch ops ctx (Yojson.Safe.to_file out_file)))
          synth_params bench
        |> ignore]
