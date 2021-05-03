open! Core
open Core_profiler.Std_offline
open Staged_synth

exception Break

let break _ = raise Break

let () = Caml.Sys.(set_signal sigint (Signal_handle break))

let print_csv_header () =
  printf
    "method,max_size,bench,n_states,n_distinct_states,n_roots,time,sol_size,gold_size,n_args,total_arg_in_degree,n_mergeable_hyper_edges,n_iters,solved\n"

let print_csv ~synth ?(bench = "") ~n_states ~time ~max_size ?sol_size
    ?gold_size ~n_args ~total_arg_in_degree ~n_distinct_states ~n_roots
    ~n_mergeable_hyper_edges ?n_iters ?solved () =
  let optional_int x = Option.map x ~f:Int.to_string |> Option.value ~default:""
  and optional_bool x =
    Option.map x ~f:(fun v -> if v then "1" else "0")
    |> Option.value ~default:""
  in
  printf "%s,%d,%s,%d,%d,%d,%f,%s,%s,%d,%d,%d,%s,%s\n" synth max_size bench
    n_states n_distinct_states n_roots (Time.Span.to_ms time)
    (optional_int sol_size) (optional_int gold_size) n_args total_arg_in_degree
    n_mergeable_hyper_edges (optional_int n_iters) (optional_bool solved)

let mergeable_hyper_edges (type t)
    (module Search_state : Search_state_intf.S with type t = t) (ss : t) =
  let open Search_state in
  let module HEdge = struct
    module T = struct
      type t = State.t * Set.M(State).t [@@deriving compare, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end in
  G.Fold.V.filter_map (graph ss) ~f:Node.to_args
  |> List.fold
       ~init:(Map.empty (module HEdge))
       ~f:(fun hedges args_v ->
         let out_v =
           G.pred (graph ss) (Node.of_args args_v)
           |> List.hd_exn |> Node.to_state_exn
         in
         let in_v =
           G.succ (graph ss) (Node.of_args args_v)
           |> List.map ~f:Node.to_state_exn
           |> Set.of_list (module State)
         in
         let h = (out_v, in_v) in
         Map.update hedges h ~f:(function None -> 1 | Some c -> c + 1))
  |> Map.length

let cad_cli =
  let module Lang = Cad_hashcode in
  let module Search_state = Search_state.Make (Lang) in
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
               let lu = st.upper |> Lang.Abs.Boxes.to_list |> List.length in
               let ll = st.lower |> Lang.Abs.Boxes.to_list |> List.length in
               [ lu; ll ])
        |> List.sort ~compare:[%compare: int]
      in
      if List.is_empty n_boxes then ()
      else (
        List.iter n_boxes ~f:(Probe.record boxes);
        Probe.record min_boxes (List.hd_exn n_boxes);
        Probe.record max_boxes (List.last_exn n_boxes);
        Probe.record median_boxes
          (List.nth_exn n_boxes (List.length n_boxes / 2)))
  end in
  let module Lazy_cegis =
    Lazy_cegis.Make (Lang) (Search_state) (Refine) (Probes)
  in
  let open Search_state in
  let run params () =
    let start = Time.now () in
    let prog, ss = Lazy_cegis.synth params in
    let end_ = Time.now () in
    let time = Time.diff end_ start in
    if params.print_csv then
      let states =
        G.Fold.V.filter_map (graph ss) ~f:Node.to_state
        |> List.map ~f:(State.state ss)
      in
      let states_distinct =
        List.filter states ~f:(fun s ->
            List.for_all states ~f:(fun s' ->
                [%compare.equal: Cad.Abs.t] s s'
                || not (Cad_abs.equiv params s s')))
      in
      let roots =
        Abs_ext.roots ~is_subset:(Cad_abs.is_subset params) states_distinct
      in
      print_csv ?bench:params.bench.filename ~synth:"cad_abs"
        ~n_states:(List.length states)
        ~n_distinct_states:(List.length states_distinct)
        ~n_roots:(List.length roots) ~time
        ?sol_size:(Option.map prog ~f:Program.size)
        ?gold_size:(Option.map params.bench.solution ~f:Program.size)
        ~max_size:params.max_cost
        ~n_args:(G.Fold.V.filter_map (graph ss) ~f:Node.to_args |> List.length)
        ~total_arg_in_degree:
          (G.Fold.V.filter (graph ss) ~f:Node.is_args
          |> List.map ~f:(G.in_degree (graph ss))
          |> List.sum (module Int) ~f:Fun.id)
        ~n_mergeable_hyper_edges:
          (mergeable_hyper_edges (module Search_state) ss)
        ()
    else
      Option.iter prog ~f:(fun solution ->
          print_s [%message (solution : Cad_op.t Program.t)])
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
      let states = G.Fold.V.filter_map (graph ss) ~f:Node.to_state in
      let states_distinct =
        List.map states ~f:(State.state ss)
        |> List.dedup_and_sort ~compare:[%compare: Cad_concrete.Abs.t]
      in
      print_csv ?bench:params.bench.filename ~synth:"cad_concrete"
        ~n_states:(List.length states)
        ~n_roots:(List.length states_distinct)
        ~n_args:(G.Fold.V.filter_map (graph ss) ~f:Node.to_args |> List.length)
        ~total_arg_in_degree:
          (G.Fold.V.filter (graph ss) ~f:Node.is_args
          |> List.map ~f:(G.in_degree (graph ss))
          |> List.sum (module Int) ~f:Fun.id)
        ~time
        ?sol_size:(Option.map prog ~f:Program.size)
        ?gold_size:(Option.map params.bench.solution ~f:Program.size)
        ~max_size:params.max_cost
        ~n_distinct_states:(List.length states_distinct)
        ~n_mergeable_hyper_edges:
          (mergeable_hyper_edges (module Search_state) ss)
        ()
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

let cad_cost_naive_cli =
  let module Lang = Cad in
  let module Synth = Cost_ordered_naive.Make (Lang) in
  let run params () =
    let start = Time.now () in
    let result = Synth.synth params in
    let end_ = Time.now () in
    let time = Time.diff end_ start in
    Option.iter result ~f:(fun () -> print_s [%message "found solution"]);
    if params.print_csv then
      print_csv ?bench:params.bench.filename ~synth:"cad_cost_naive_hamming"
        ~n_states:(-1) ~n_distinct_states:(-1) ~n_roots:(-1) ~time
        ~max_size:params.max_cost ~n_args:(-1) ~total_arg_in_degree:(-1)
        ~n_mergeable_hyper_edges:(-1) ()
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

let cad_sample_naive_cli =
  let module Lang = Cad in
  let module Synth = Sampling_naive.Make (Lang) in
  let run params () =
    let stats = Synth.create_stats () in
    let start = Time.now () in
    (try Synth.synth params stats with Break -> ());
    let end_ = Time.now () in
    let time = Time.diff end_ start in
    if params.print_csv then
      print_csv ?bench:params.bench.filename ~synth:"cad_sample_naive"
        ~n_states:stats.n_states ~n_iters:stats.n_iters ~n_distinct_states:(-1)
        ~n_roots:(-1) ~time ~max_size:params.max_cost ~n_args:(-1)
        ~total_arg_in_degree:(-1) ~n_mergeable_hyper_edges:(-1)
        ~solved:stats.solved ()
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

let cad_sample_diverse_cli =
  let module Lang = Cad in
  let module Synth = Sampling_diverse.Make (Lang) in
  let run params () =
    let stats = Synth.create_stats () in
    let start = Time.now () in
    (try Synth.synth params stats with Break -> ());
    let end_ = Time.now () in
    let time = Time.diff end_ start in
    if params.print_csv then
      print_csv ?bench:params.bench.filename ~synth:"cad_sample_diverse"
        ~n_states:stats.n_states ~n_iters:stats.n_iters ~n_distinct_states:(-1)
        ~n_roots:(-1) ~time ~max_size:params.max_cost ~n_args:(-1)
        ~total_arg_in_degree:(-1) ~n_mergeable_hyper_edges:(-1)
        ~solved:stats.solved ()
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

let cad_sample_diverse_vp_cli =
  let module Lang = Cad in
  let module Synth = Sampling_diverse_vp.Make (Lang) in
  let run params () =
    let stats = Synth.create_stats () in
    let start = Time.now () in
    (try Synth.synth params stats with Break -> ());
    let end_ = Time.now () in
    let time = Time.diff end_ start in
    if params.print_csv then
      print_csv ?bench:params.bench.filename ~synth:"cad_sample_diverse_vp"
        ~n_states:stats.n_states ~n_iters:stats.n_iters ~n_distinct_states:(-1)
        ~n_roots:(-1) ~time ~max_size:params.max_cost ~n_args:(-1)
        ~total_arg_in_degree:(-1) ~n_mergeable_hyper_edges:(-1)
        ~solved:stats.solved ()
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
  Memtrace.trace_if_requested ();
  Command.group ~summary:"Run lazy CEGIS."
    [
      (* ("cad", csg_cli); *)
      ("cad-abs", cad_cli);
      ("cad-concrete", cad_concrete_cli);
      ("cad-cost-naive", cad_cost_naive_cli);
      ("cad-sample-naive", cad_sample_naive_cli);
      ("cad-sample-diverse", cad_sample_diverse_cli);
      ("cad-sample-diverse-vp", cad_sample_diverse_vp_cli);
    ]
  |> Command.run
