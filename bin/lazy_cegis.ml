open! Core
open Staged_synth.Lazy_cegis

let _inputs, _output =
  let conv l =
    List.map l ~f:(fun i -> if i = 0 then false else true) |> Array.of_list
  in
  let inputs =
    List.map ~f:conv [ [ 0; 1; 1; 0 ]; [ 1; 1; 0; 0 ]; [ 0; 0; 0; 1 ] ]
  in
  let output = conv [ 0; 1; 0; 1 ] in
  (inputs, output)

let main ~n ~enable_graph_output ~seed ~max_cost ~k ~print_header ~abstraction
    ~check () =
  if print_header then (
    Fmt.pr
      "k,n,seed,max_cost,abstraction,n_nodes,n_covered,n_refuted,min_width,max_width,median_width,check,sat\n";
    exit 0 );

  let no_abstraction = abstraction = 0 in

  enable_dump := enable_graph_output;

  let state = Random.State.make [| seed |] in
  let inputs =
    List.init n ~f:(fun _ -> Array.init k ~f:(fun _ -> Random.State.bool state))
  in
  let output = Array.init k ~f:(fun _ -> Random.State.bool state) in
  let graph, stats = synth ~max_cost ~no_abstraction inputs output in
  let check_output =
    if check && not stats.sat then Some (check_search_space inputs graph)
    else None
  in
  Fmt.pr "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%s,%d\n" k n seed max_cost
    abstraction stats.Stats.n_nodes stats.Stats.n_covered stats.Stats.n_refuted
    stats.Stats.min_width stats.Stats.max_width stats.Stats.median_width
    ( match check_output with
    | Some (Ok ()) -> "1"
    | Some (Error _) -> "0"
    | None -> "" )
    (if stats.Stats.sat then 1 else 0)

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Run lazy CEGIS on a random testcase."
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
      and k = anon ("num-bits" %: int)
      and print_header = flag "print-header" no_arg ~doc:" print csv header"
      and abstraction =
        flag "abstraction"
          (optional_with_default 1 int)
          ~doc:" set to 0 to disable abstraction refinement"
      and check =
        flag "check" no_arg
          ~doc:" check the search space by sampling random programs"
      in

      main ~n ~enable_graph_output ~seed ~max_cost ~k ~print_header ~abstraction
        ~check]
  |> Command.run