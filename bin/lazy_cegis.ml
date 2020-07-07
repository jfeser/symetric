open! Core
open Staged_synth.Lazy_cegis

let main ~n ~enable_graph_output ~seed ~max_cost ~print_header ~abstraction
    ~check ~refine () =
  if print_header then (
    Fmt.pr
      "k,n,seed,max_cost,abstraction,n_state_nodes,n_arg_nodes,n_covered,n_refuted,min_width,max_width,median_width,check,sat,refine\n";
    exit 0 );

  let no_abstraction = abstraction = 0 in
  (refine_strategy :=
     match refine with
     | "first" -> `First
     | "random" -> `Random
     | "pareto" -> `Pareto
     | _ -> failwith @@ Fmt.str "unknown refinement strategy: %s" refine);

  enable_dump := enable_graph_output;
  max_size := max_cost;

  let bench = Sexp.input_sexp In_channel.stdin |> [%of_sexp: Cad_bench.t] in
  let graph, stats = synth ~no_abstraction bench in

  let check_output =
    if check && not stats.sat then
      (* Some (check_search_space inputs graph) *)
      failwith "unimplemented"
    else None
  in
  Fmt.pr "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%s,%d,%s\n" n seed max_cost
    abstraction stats.Stats.n_state_nodes stats.Stats.n_arg_nodes
    stats.Stats.n_covered stats.Stats.n_refuted stats.Stats.min_width
    stats.Stats.max_width stats.Stats.median_width
    ( match check_output with
    | Some (Ok ()) -> "1"
    | Some (Error _) -> "0"
    | None -> "" )
    (if stats.Stats.sat then 1 else 0)
    refine

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Run lazy CEGIS on a CAD benchmark."
    [%map_open
      let n =
        flag "num-inputs"
          (optional_with_default 2 int)
          ~doc:" number of function inputs"
      and enable_graph_output =
        flag "output-graph" no_arg ~doc:" enable output of dot graphs"
      and seed = flag "seed" (optional_with_default 0 int) ~doc:" random seed"
      and max_cost =
        flag "max-cost"
          (optional_with_default 20 int)
          ~doc:" maximum program cost"
      and print_header = flag "print-header" no_arg ~doc:" print csv header"
      and abstraction =
        flag "abstraction"
          (optional_with_default 1 int)
          ~doc:" set to 0 to disable abstraction refinement"
      and check =
        flag "check" no_arg
          ~doc:" check the search space by sampling random programs"
      and refine =
        flag "refine"
          (optional_with_default "first" string)
          ~doc:" refinement strategy"
      in

      main ~n ~enable_graph_output ~seed ~max_cost ~print_header ~abstraction
        ~check ~refine]
  |> Command.run
