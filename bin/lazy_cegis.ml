open! Core
open Core_profiler.Std_offline
open Staged_synth

let () = Signal.Expert.handle Signal.int (fun _ -> exit 1)

let print_header () =
  Fmt.pr
    "k,n,seed,max_cost,abstraction,n_state_nodes,n_arg_nodes,n_covered,n_refuted,min_width,max_width,median_width,check,sat\n"

let csg_cli =
  let open Command.Let_syntax in
  Command.basic ~summary:"Synthesize a CAD program using lazy cegis."
    [%map_open
      let params =
        Params.cli
          [%map_open
            let bench_fn = anon ("bench" %: string) in
            Sexp.load_sexp_conv_exn bench_fn [%of_sexp: Csg.Bench.t]]
          (Command.Param.return ())
      in
      let module Search_state = Search_state.Make (Csg) in
      let module Refine = Interp_refine.Make (Csg) (Search_state) in
      let module Probes = struct
        let fill = None
      end in
      let module Lazy_cegis =
        Lazy_cegis.Make (Csg) (Search_state) (Refine) (Probes)
      in
      fun () -> (Lazy_cegis.synth params : _ * Search_state.t) |> ignore]

let print_csv ~synth ?(bench = "") ~n_states ~time ~max_size ~sol_size
    ~gold_size =
  let optional_int x =
    Option.map x ~f:Int.to_string |> Option.value ~default:""
  in
  printf "%s,%d,%s,%d,%f,%s,%s\n" synth max_size bench n_states
    (Time.Span.to_ms time) (optional_int sol_size) (optional_int gold_size)

let cad_cli =
  let module Search_state = Search_state.Make (Cad) in
  let module Refine = Backtrack_refine.Make (Search_state) in
  let module Probes = struct
    open Search_state

    let boxes = Probe.create ~name:"boxes" ~units:Profiler_units.Int

    let min_boxes = Probe.create ~name:"min_boxes" ~units:Profiler_units.Int

    let max_boxes = Probe.create ~name:"max_boxes" ~units:Profiler_units.Int

    let median_boxes =
      Probe.create ~name:"median_boxes" ~units:Profiler_units.Int

    let fill =
      Option.some @@ fun ss _ ->
      let n_boxes =
        G.Fold.V.filter_map (graph ss) ~f:Node.to_state
        |> List.concat_map ~f:(fun s ->
               let st = State.state ss s in
               let lu = st.upper |> Cad.Abs.Boxes.to_list |> List.length in
               let ll = st.lower |> Cad.Abs.Boxes.to_list |> List.length in
               [ lu; ll ])
        |> List.sort ~compare:[%compare: int]
      in
      if List.is_empty n_boxes then ()
      else (
        List.iter n_boxes ~f:(Probe.record boxes);
        Probe.record min_boxes (List.hd_exn n_boxes);
        Probe.record max_boxes (List.last_exn n_boxes);
        Probe.record median_boxes
          (List.nth_exn n_boxes (List.length n_boxes / 2)) )
  end in
  let module Lazy_cegis = Lazy_cegis.Make (Cad) (Search_state) (Refine) (Probes)
  in
  let open Search_state in
  let run params () =
    let start = Time.now () in
    let prog, ss = Lazy_cegis.synth params in
    let end_ = Time.now () in
    let time = Time.diff end_ start in
    if params.print_csv then
      print_csv ?bench:params.bench.filename ~synth:"cad_abs"
        ~n_states:
          (G.Fold.V.filter_map (graph ss) ~f:Node.to_state |> List.length)
        ~time
        ~sol_size:(Option.map prog ~f:Program.size)
        ~gold_size:(Option.map params.bench.solution ~f:Program.size)
        ~max_size:params.max_cost
    else
      Option.iter params.bench.solution ~f:(fun ground_truth ->
          print_s [%message (ground_truth : Cad_op.t Program.t)])
  in

  let open Command.Let_syntax in
  Command.basic ~summary:"Synthesize a 2D CAD program using lazy cegis."
    [%map_open
      let params =
        Params.cli
          [%map_open
            let bench_fn = anon ("bench" %: string) in
            Cad.Bench.load bench_fn]
          Cad_params.cli
      in
      run params]

let cad_concrete_cli =
  let module Search_state = Search_state.Make (Cad_concrete) in
  let module Refine = Dummy_refine.Make (Search_state) in
  let module Probes = struct
    let fill = None
  end in
  let module Lazy_cegis =
    Lazy_cegis.Make (Cad_concrete) (Search_state) (Refine) (Probes)
  in
  let open Search_state in
  let run params () =
    let start = Time.now () in
    let prog, ss = Lazy_cegis.synth params in
    let end_ = Time.now () in
    let time = Time.diff end_ start in
    if params.print_csv then
      print_csv ?bench:params.bench.filename ~synth:"cad_concrete"
        ~n_states:
          (G.Fold.V.filter_map (graph ss) ~f:Node.to_state |> List.length)
        ~time
        ~sol_size:(Option.map prog ~f:Program.size)
        ~gold_size:(Option.map params.bench.solution ~f:Program.size)
        ~max_size:params.max_cost
    else
      Option.iter params.bench.solution ~f:(fun ground_truth ->
          print_s [%message (ground_truth : Cad_op.t Program.t)])
  in

  let open Command.Let_syntax in
  Command.basic ~summary:"Synthesize a 2D CAD program using lazy cegis."
    [%map_open
      let params =
        Params.cli
          [%map_open
            let bench_fn = anon ("bench" %: string) in
            Cad.Bench.load bench_fn]
          Cad_params.cli
      in
      run params]

let () =
  Command.group ~summary:"Run lazy CEGIS."
    [
      ( "header",
        Command.basic ~summary:"Print stats header."
          (Command.Param.return print_header) );
      ("cad", csg_cli);
      ("cad-abs", cad_cli);
      ("cad-concrete", cad_concrete_cli);
      (* ( "random",
       *   Command.basic ~summary:"Run lazy CEGIS on a random testcase."
       *     [%map_open
       *       let n =
       *         flag "num-inputs"
       *           (optional_with_default 2 int)
       *           ~doc:" number of function inputs"
       *       and seed =
       *         flag "seed" (optional_with_default 0 int) ~doc:" random seed"
       *       and n_bits = anon ("num-bits" %: int)
       *       and check =
       *         flag "check" no_arg
       *           ~doc:" check the search space by sampling random programs"
       *       and max_cost, print_header = shared in
       * 
       *       random ~n ~seed ~k:n_bits ~print_header ~check] ); *)
    ]
  |> Command.run
