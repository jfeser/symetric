let synthesize ?log (synth_params : Baseline.Params.t) target =
  Baseline.synthesize ?log (module Tower) synth_params (`Value target)

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve tower problems with enumeration."
    [%map_open
      let out = flag "-out" (optional string) ~doc:" output file"
      and synth_params = Baseline.Params.param in
      fun () ->
        let target =
          Sexp.input_sexp In_channel.stdin |> Tower.parse |> Program.eval Tower.Value.eval
        in
        synthesize ?log:(Option.map out ~f:Yojson.Safe.to_file) synth_params target
        |> ignore]
