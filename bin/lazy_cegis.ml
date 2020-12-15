open! Core
open Staged_synth
open Lazy_cegis_old
open Ast
open Params

let print_header () =
  Fmt.pr
    "k,n,seed,max_cost,abstraction,n_state_nodes,n_arg_nodes,n_covered,n_refuted,min_width,max_width,median_width,check,sat\n"

let print_stats ?(check_output = None) ?(k = -1) ?(n = -1) ?(seed = -1) params
    stats =
  Fmt.pr "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%s,%d\n" k n seed params.max_cost
    stats.Stats.n_state_nodes stats.Stats.n_arg_nodes stats.Stats.n_covered
    stats.Stats.n_refuted stats.Stats.min_width stats.Stats.max_width
    stats.Stats.median_width
    ( match check_output with
    | Some (Ok ()) -> "1"
    | Some (Error _) -> "0"
    | None -> "" )
    (if stats.Stats.sat then 1 else 0)

let random ~n ~seed ~k ~check () = failwith "Unimplemented"

(* let state = Random.State.make [| seed |] in
 * let inputs, output = failwith "unimplemented" in
 * List.iteri inputs ~f:(fun i v -> Fmt.epr "Input %d: %a\n" i Conc.pp v);
 * Fmt.epr "Output: %a\n" Conc.pp output;
 * 
 * let search_state, stats = synth inputs output in
 * 
 * let check_output =
 *   None
 *   (\* if check && not stats.sat then Some (check_search_space inputs search_state)
 *    * else None *\)
 * in
 * print_stats ~k ~n ~seed ~check_output stats *)

let cad create_params bench () =
  (create_params bench |> synth : Search_state.t * Stats.t) |> ignore

let () =
  let open Command.Let_syntax in
  let create_params =
    [%map_open
      let enable_dump =
        flag "output-graph" no_arg ~doc:" enable output of dot graphs"
      and hide_values =
        flag "hide-values" no_arg ~doc:" hide abstract values in graph output"
      and enable_forced_bit_check =
        flag "enable-forced-bit-check" no_arg
          ~doc:" enable checking for forced bits when refining"
      and max_cost =
        flag "max-cost"
          (optional_with_default 20 int)
          ~doc:" maximum program cost"
      in

      Params.create ~enable_dump ~max_cost ~enable_forced_bit_check ~hide_values]
  in
  Command.group ~summary:"Run lazy CEGIS."
    [
      ( "header",
        Command.basic ~summary:"Print stats header."
          (Command.Param.return print_header) );
      ( "cad",
        Command.basic ~summary:"Synthesize a CAD program."
          [%map_open
            let create_params = create_params
            and bench_fn = anon ("bench" %: string) in
            let bench = Sexp.load_sexp_conv_exn bench_fn [%of_sexp: Bench.t] in
            cad create_params bench] );
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
