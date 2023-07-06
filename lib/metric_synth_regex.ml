open Regex

module Params = struct
  type t = { max_int : int } [@@deriving yojson]

  let default_max_int = 15
  let create ?(max_int = default_max_int) () = { max_int }

  let param =
    let open Command.Let_syntax in
    [%map_open
      let max_int =
        flag "max-int"
          (optional_with_default default_max_int int)
          ~doc:"maximum integer constant in regex"
      in
      create ~max_int ()]
end

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

let rewrite : int -> Op.t P.t -> Op.t P.t list =
 fun max_int -> function
  | Apply (Int x, []) ->
      if x <= 1 then [ Apply (Int (x + 1), []) ]
      else if x >= max_int then [ Apply (Int (x - 1), []) ]
      else [ Apply (Int (x + 1), []); Apply (Int (x - 1), []) ]
  | Apply (Repeat, [ r; n ]) -> [ Apply (Repeat_range, [ r; n; n ]) ]
  | _ -> []

let synthesize ?log (metric_params : Metric_synth.Params.t) (dsl_params : Params.t)
    (vctx, operators) =
  let module Dsl = struct
    include Regex

    module Value = struct
      include Value

      let eval = eval vctx
      let target_distance = target_distance vctx
    end

    let parse = parse
    let serialize = serialize
    let rewrite = rewrite dsl_params.max_int
    let operators = operators @ Op.default_operators dsl_params.max_int
  end in
  Metric_synth.synthesize ?log metric_params (module Dsl)

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve regex problems with enumeration."
    [%map_open
      let synth_params = Metric_synth.Params.param
      and dsl_params = Params.param
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
          synth_params dsl_params bench
        |> ignore]
