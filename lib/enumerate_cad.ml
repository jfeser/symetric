open Std

let mk_log dsl_params output synth_log =
  output (`Assoc [ ("synth", synth_log); ("dsl", Cad_ext.Params.yojson_of_t dsl_params) ])

let synthesize ?log (synth_params : Baseline.Params.t) (dsl_params : Cad_ext.Params.t)
    target =
  let module Dsl = struct
    include Cad_ext

    module Value = struct
      include Value

      let eval = eval ~error_on_trivial:true ~dim:dsl_params.dim
    end

    let operators =
      Cad_ext.Op.default_operators ~xres:dsl_params.dim.xres ~yres:dsl_params.dim.yres
  end in
  Baseline.synthesize
    ?log:(Option.map log ~f:(mk_log dsl_params))
    (module Dsl)
    synth_params (`Value target)

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve CAD problems with enumeration."
    [%map_open
      let synth_params = Baseline.Params.param
      and dsl_params = Cad_ext.Params.param
      and out = flag "-out" (optional string) ~doc:" output file" in
      fun () ->
        let prog = Cad_ext.parse @@ Sexp.input_sexp In_channel.stdin in
        let target =
          Program.eval
            (Cad_ext.Value.eval ~error_on_trivial:false ~dim:dsl_params.dim)
            prog
        in
        synthesize
          ?log:(Option.map out ~f:Yojson.Safe.to_file)
          synth_params dsl_params target
        |> ignore]
