open Tower

let rewrite : Op.t P.t -> Op.t P.t list = function
  | Apply (Int x, []) ->
      if x < 1 then [ Apply (Int (x + 1), []) ]
      else if x > 8 then [ Apply (Int (x - 1), []) ]
      else [ Apply (Int (x + 1), []); Apply (Int (x - 1), []) ]
  | Apply (Move_s x, args) ->
      let ret = [] in
      let ret = if x > -8 then P.Apply (Op.Move_s (x - 1), args) :: ret else ret in
      let ret = if x < 8 then P.Apply (Op.Move_s (x + 1), args) :: ret else ret in
      ret
  | Apply (Move_p x, args) ->
      let ret = [] in
      let ret = if x > -8 then P.Apply (Op.Move_p (x - 1), args) :: ret else ret in
      let ret = if x < 8 then P.Apply (Op.Move_p (x + 1), args) :: ret else ret in
      ret
  | _ -> []

let synthesize ?log (metric_params : Metric_synth.Params.t) target =
  let ctx = Value.Ctx.create ~target () in
  let module Dsl = struct
    include Tower

    module Value = struct
      include Value

      let target_distance = target_distance ctx
      let pp = Fmt.nop
    end

    let rewrite = rewrite
  end in
  Metric_synth.synthesize ?log metric_params (module Dsl)

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve tower problems with metric synthesis."
    [%map_open
      let out = flag "-out" (optional string) ~doc:" output file"
      and synth_params = Metric_synth.Params.param in
      fun () ->
        let target =
          Sexp.input_sexp In_channel.stdin |> Tower.parse |> Program.eval Tower.Value.eval
        in
        synthesize ?log:(Option.map out ~f:Yojson.Safe.to_file) synth_params target
        |> ignore]
