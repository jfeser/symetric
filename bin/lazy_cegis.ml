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

let cad_sample_naive_cli =
  let module Lang = Cad in
  let module Synth = Sampling_naive.Make (Lang) in
  let run params () =
    if Params.(get params print_csv_header) then
      print_endline @@ Dumb_params.csv_header params
    else
      let start = Time.now () in
      (try Synth.synth params with Break -> ());
      let end_ = Time.now () in
      Params.(get params runtime) := Time.diff end_ start;
      if Params.(get params print_csv) then
        print_endline @@ Dumb_params.csv params
  in
  let spec = Lang.spec @ Params.spec @ Sampling_naive.spec in
  let open Command.Let_syntax in
  Command.basic ~summary:""
    [%map_open
      let params = Dumb_params.cli spec in
      run params]

module Hamming_dist = struct
  let value c c' = Cad.Value.hamming c c' |> Float.of_int
end

module Jaccard_dist = struct
  let value c c' = Cad.Value.jaccard c c'
end

module Zs_dist = struct
  let program (p : Cad.Op.t Program.t) (p' : Cad.Op.t Program.t) =
    Tree_dist.zhang_sasha ~eq:[%compare.equal: Cad.Op.t] p p' |> Float.of_int
end

let rec norm = function
  | Program.Apply (((Cad.Op.Union | Inter) as op), args) ->
      let args' =
        List.sort ~compare:[%compare: Cad.Op.t Program.t]
        @@ List.map ~f:norm args
      in
      Apply (op, args')
  | Apply (op, args) ->
      let args' = List.map ~f:norm args in
      Apply (op, args')

module Norm_zs_dist = struct
  let program (p : Cad.Op.t Program.t) (p' : Cad.Op.t Program.t) =
    Tree_dist.zhang_sasha ~eq:[%compare.equal: Cad.Op.t] (norm p) (norm p')
    |> Float.of_int
end

module Norm_tree_ball_dist = struct
  let program (p : Cad.Op.t Program.t) (p' : Cad.Op.t Program.t) =
    Tree_ball.dist ~compare:[%compare: Cad.Op.t] (norm p) (norm p')
end

module Jaccard_edge_dist = struct
  let value c c' = Cad.Value.jaccard (Cad.Value.edges c) (Cad.Value.edges c')
end

module Dist = struct
  include Jaccard_dist
  include Norm_zs_dist
end

let cad_sample_diverse_cli =
  let module Lang = Cad in
  let module Synth = Sampling_diverse.Make (Lang) (Dist) in
  let run params () =
    if Params.(get params print_csv_header) then
      print_endline @@ Dumb_params.csv_header params
    else (
      Random.set_state @@ Random.State.make [| Params.(get params seed) |];
      let start = Time.now () in
      (try Synth.synth params with Break -> ());
      let end_ = Time.now () in
      Params.(get params runtime) := Time.diff end_ start;
      if Params.(get params print_csv) then
        print_endline @@ Dumb_params.csv params)
  in
  let spec = Lang.spec @ Params.spec @ Sampling_diverse.spec in
  let open Command.Let_syntax in
  Command.basic ~summary:""
    [%map_open
      let params = Dumb_params.cli spec in
      run params]

let cad_baseline_cli =
  let module Lang = Cad in
  let module Validate = struct
    let validate = ignore
  end in
  let module Synth = Baseline.Make (Lang) (Dist) (Validate) in
  let run params () =
    if Params.(get params print_csv_header) then
      print_endline @@ Dumb_params.csv_header params
    else (
      Random.set_state @@ Random.State.make [| Params.(get params seed) |];
      let start = Time.now () in
      (try Synth.synth params with Break -> ());
      let end_ = Time.now () in
      Params.(get params runtime) := Time.diff end_ start;
      if Params.(get params print_csv) then
        print_endline @@ Dumb_params.csv params)
  in
  let spec = Lang.spec @ Params.spec in
  let open Command.Let_syntax in
  Command.basic ~summary:""
    [%map_open
      let params = Dumb_params.cli spec in
      run params]

let cad_baseline_term_cli =
  let module Lang = Cad_term in
  let module P = Program.Make (Lang.Op) in
  let module Validate = struct
    let validate vs =
      let values =
        List.map vs ~f:(fun (v, _, _) -> v) |> Set.of_list (module Lang.Value)
      in
      Set.iter values
        ~f:([%test_pred: Lang.Value.t] (fun v -> Set.mem values (norm v)))

    let validate vs = timed "validate" (fun () -> validate vs)
  end in
  let run params () =
    if Params.(get params print_csv_header) then
      print_endline @@ Dumb_params.csv_header params
    else (
      Random.set_state @@ Random.State.make [| Params.(get params seed) |];
      let eval = P.eval_memoized (Cad_conc.eval params) in
      let module Dist = struct
        let program = Zs_dist.program

        let value p p' =
          let v = eval p and v' = eval p' in
          Hamming_dist.value v v'
      end in
      let module Synth = Baseline.Make (Lang) (Dist) (Validate) in
      let start = Time.now () in
      (try Synth.synth params with Break -> ());
      let end_ = Time.now () in
      Params.(get params runtime) := Time.diff end_ start;
      if Params.(get params print_csv) then
        print_endline @@ Dumb_params.csv params)
  in

  let spec = Lang.spec @ Params.spec in
  let open Command.Let_syntax in
  Command.basic ~summary:""
    [%map_open
      let params = Dumb_params.cli spec in
      run params]

let () =
  Command.group ~summary:"Run lazy CEGIS."
    [
      ("cad-sample-naive", cad_sample_naive_cli);
      ("cad-sample-diverse", cad_sample_diverse_cli);
      ("cad-baseline", cad_baseline_cli);
      ("cad-baseline-term", cad_baseline_term_cli);
    ]
  |> Command.run
