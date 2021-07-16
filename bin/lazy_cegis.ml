open! Core
open Staged_synth

exception Break

let break _ = raise Break

let timed n f =
  let start = Time.now () in
  let ret = f () in
  let end_ = Time.now () in
  Fmt.epr "ran %s in %a\n%!" n Time.Span.pp (Time.diff end_ start);
  ret

let () = Caml.Sys.(set_signal sigint (Signal_handle break))

let print_json = Yojson.Basic.to_channel Out_channel.stdout

let run_synth synth params () =
  Random.set_state @@ Random.State.make [| Params.(get params seed) |];
  let start = Time.now () in
  (try synth params with Break -> ());
  let end_ = Time.now () in
  Params.(get params runtime) := Time.diff end_ start;
  if Params.(get params print_json) then print_json @@ Dumb_params.json params

let baseline_cli (type value op) (module Lang : Lang_intf.S with type Value.t = value and type Op.t = op) =
  let module Validate = struct
    let validate = ignore
  end in
  let module Synth = Baseline.Make (Lang) (Validate) in
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; Baseline.spec ] in
  let open Command.Let_syntax in
  Command.basic ~summary:(sprintf "Enumerative baseline for %s" Lang.name)
  @@ [%map_open
       let params = Dumb_params.Spec.cli spec in
       run_synth Synth.synth params]

let diverse_cli (type value op) (module Lang : Lang_intf.S with type Value.t = value and type Op.t = op) =
  let module Synth = Sampling_diverse.Make (Lang) in
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; Sampling_diverse.spec ] in
  let open Command.Let_syntax in
  Command.basic
    ~summary:(sprintf "Diversity sampling for %s" Lang.name)
    [%map_open
      let params = Dumb_params.Spec.cli spec in
      run_synth Synth.synth params]

let cad_sample_naive_cli =
  let module Lang = Cad in
  let module Synth = Sampling_naive.Make (Lang) in
  let run params () =
    let start = Time.now () in
    (try Synth.synth params with Break -> ());
    let end_ = Time.now () in
    Params.(get params runtime) := Time.diff end_ start;
    if Params.(get params print_json) then print_json @@ Dumb_params.json params
  in
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; Sampling_naive.spec ] in
  let open Command.Let_syntax in
  Command.basic ~summary:""
    [%map_open
      let params = Dumb_params.Spec.cli spec in
      run params]

let cad_sample_naive_ball_cli =
  let module Lang = Cad in
  let module Synth = Sampling_naive_ball.Make (Lang) in
  let run params () =
    Random.set_state @@ Random.State.make [| Params.(get params seed) |];
    let start = Time.now () in
    (try Synth.synth params with Break -> ());
    let end_ = Time.now () in
    Params.(get params runtime) := Time.diff end_ start;
    if Params.(get params print_json) then print_json @@ Dumb_params.json params
  in
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; Sampling_naive_ball.spec ] in
  let open Command.Let_syntax in
  Command.basic ~summary:""
    [%map_open
      let params = Dumb_params.Spec.cli spec in
      run params]

let cad_sample_learn_cli =
  let module Lang = Cad in
  let module Synth = Soft_refinement.Make (Lang) in
  let run params () =
    Random.set_state @@ Random.State.make [| Params.(get params seed) |];
    let start = Time.now () in
    (try Synth.synth params with Break -> ());
    let end_ = Time.now () in
    Params.(get params runtime) := Time.diff end_ start;
    if Params.(get params print_json) then print_json @@ Dumb_params.json params
  in
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; Soft_refinement.spec ] in
  let open Command.Let_syntax in
  Command.basic ~summary:""
    [%map_open
      let params = Dumb_params.Spec.cli spec in
      run params]

let () =
  Command.group ~summary:"Run lazy CEGIS."
    [
      ("cad-sample-naive", cad_sample_naive_cli);
      ("cad-sample-naive-ball", cad_sample_naive_ball_cli);
      ("cad-sample-learn", cad_sample_learn_cli);
      ("cad-sample-diverse", diverse_cli (module Cad));
      ("cad-baseline", baseline_cli (module Cad));
      ("tensor-baseline", baseline_cli (module Tensor));
      ("tensor-sample-diverse", diverse_cli (module Tensor));
      ("cad-term-baseline", baseline_cli (module Cad_term));
    ]
  |> Command.run
