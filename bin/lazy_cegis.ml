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
      fun () -> (Lazy_cegis.synth params : Search_state.t) |> ignore]

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
        |> List.map ~f:(fun s ->
               State.state ss s |> Cad.Abs.Boxes.to_list |> List.length)
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
  let run params () =
    (Lazy_cegis.synth params : Search_state.t) |> ignore;
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
            Sexp.load_sexp_conv_exn bench_fn [%of_sexp: Cad.Bench.t]]
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
      ("cad2", cad_cli);
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
