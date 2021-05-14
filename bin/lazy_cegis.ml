open! Core
open Core_profiler.Std_offline
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

let print_csv_header () =
  printf
    "method,max_size,bench,n_states,n_distinct_states,n_roots,time,sol_size,gold_size,n_args,total_arg_in_degree,n_mergeable_hyper_edges,n_iters,solved\n"

let print_csv ~synth ?(bench = "") ?n_states ?time ?max_size ?sol_size
    ?gold_size ?n_args ?total_arg_in_degree ?n_distinct_states ?n_roots
    ?n_mergeable_hyper_edges ?n_iters ?solved () =
  let optional_int x = Option.map x ~f:Int.to_string |> Option.value ~default:""
  and optional_bool x =
    Option.map x ~f:(fun v -> if v then "1" else "0")
    |> Option.value ~default:""
  and optional_span x =
    Option.map x ~f:(fun x -> Time.Span.to_ms x |> sprintf "%f")
    |> Option.value ~default:""
  in
  printf "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n" synth
    (optional_int max_size) bench (optional_int n_states)
    (optional_int n_distinct_states)
    (optional_int n_roots) (optional_span time) (optional_int sol_size)
    (optional_int gold_size) (optional_int n_args)
    (optional_int total_arg_in_degree)
    (optional_int n_mergeable_hyper_edges)
    (optional_int n_iters) (optional_bool solved)

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

module Hamming_dist = struct
  let value (params : Cad.params) (c : Cad.Value.t) (c' : Cad.Value.t) =
    let ct = ref 0 in
    for x = 0 to params.bench.input.xmax - 1 do
      for y = 0 to params.bench.input.ymax - 1 do
        let p =
          Vector2.{ x = Float.of_int x +. 0.5; y = Float.of_int y +. 0.5 }
        in
        ct :=
          !ct
          +
          if
            let v = Map.find_exn c p and v' = Map.find_exn c' p in
            Bool.(v = v')
          then 0
          else 1
      done
    done;
    Float.of_int !ct
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

module Dist = struct
  include Hamming_dist
  include Norm_zs_dist
end

let cad_simple_cli =
  let module Lang = Cad in
  let module Synth = Sampling_enum.Make (Lang) (Dist) in
  let run params () =
    let start = Time.now () in
    (try Synth.synth params with Break -> ());
    let end_ = Time.now () in
    let time = Time.diff end_ start in
    if params.print_csv then
      print_csv ?bench:params.bench.filename ~synth:"cad_simple_enum" ~time
        ~max_size:params.max_cost ()
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

let cad_baseline_cli =
  let module Lang = Cad in
  let module Validate = struct
    let validate = ignore
  end in
  let module Synth = Baseline.Make (Lang) (Dist) (Validate) in
  let run params () =
    let start = Time.now () in
    (try Synth.synth params with Break -> ());
    let end_ = Time.now () in
    let time = Time.diff end_ start in
    if params.print_csv then
      print_csv ?bench:params.bench.filename ~synth:"cad_baseline" ~time
        ~max_size:params.max_cost ()
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
    let eval = P.eval_memoized (Cad_conc.eval params) in
    let module Dist = struct
      let program = Zs_dist.program

      let value params p p' =
        let v = eval p and v' = eval p' in
        Hamming_dist.value params v v'
    end in
    let module Synth = Baseline.Make (Lang) (Dist) (Validate) in
    let start = Time.now () in
    (try Synth.synth params with Break -> ());
    let end_ = Time.now () in
    let time = Time.diff end_ start in
    if params.print_csv then
      print_csv ?bench:params.bench.filename ~synth:"cad_baseline_term" ~time
        ~max_size:params.max_cost ()
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
      ("cad-sample-naive", cad_sample_naive_cli);
      ("cad-sample-diverse", cad_sample_diverse_cli);
      ("cad-sample-diverse-vp", cad_sample_diverse_vp_cli);
      ("cad-simple", cad_simple_cli);
      ("cad-baseline", cad_baseline_cli);
      ("cad-baseline-term", cad_baseline_term_cli);
    ]
  |> Command.run
