open Core
open Cad
module Synth = Local_search_diverse.Make (Cad)

let make_rules rules_fn rule_sets ops =
  let open Local_search in
  let rules =
    if String.(rules_fn <> "") then
      Sexp.load_sexp_conv_exn rules_fn [%of_sexp: (Op.t Rule.t * float * float * float) list]
      |> List.map ~f:(fun (r, _, _, _) -> r)
    else if String.(rule_sets <> "") then
      let rule_types = String.split rule_sets ~on:',' in

      let open Pattern in
      List.concat_map rule_types ~f:(function
        | "close-leaf" -> close_leaf_patterns ops
        | "union-leaf" -> union_leaf_patterns ops
        | "push-pull-replicate" -> push_pull_replicate ops
        | "leaf" -> leaf_patterns (module Op) ops
        | "rename" -> rename_patterns (module Op) ops
        | class_ -> raise_s [%message "unknown rule class" (class_ : string)])
    else []
  in
  List.dedup_and_sort ~compare:[%compare: Op.t Rule.t] rules

let make_dist = function
  | "jaccard" -> Cad_conc.jaccard
  | "feature" -> Cad_conc.feature_dist
  | dist -> raise_s [%message "unknown distance" (dist : string)]

let make_search_thresh str = Sexp.of_string_conv_exn str [%of_sexp: Local_search_diverse.Search_thresh.t]

(* let cli =
 *   let spec = Dumb_params.Spec.union [ spec; Params.spec; spec ] in
 *   let open Command.Let_syntax in
 *   Command.basic ~summary:(sprintf "Diversity sampling for %s" name)
 *   @@ [%map_open
 *        let params = Dumb_params.Spec.cli spec in
 *        Synth_utils.run_synth
 *          (fun params ->
 *            let open Local_search_diverse in
 *            let bench = Params.get params bench in
 *            let ops = Bench.ops bench
 *            and output = Bench.output bench
 *            and ectx = Cad.Value.Ctx.create ~xlen:bench.input.xmax ~ylen:bench.input.ymax () in
 *            let ctx =
 *              Synth.Ctx.create
 *                ~search_thresh:(make_search_thresh @@ Params.get params search_thresh)
 *                ~rules:(make_rules (Params.get params rules_fn) (Params.get params rule_sets) ops)
 *                ~distance:(make_dist @@ Params.get params distance)
 *                ~verbose:(Params.get params Baseline.verbose) ~max_cost:(Params.get params Baseline.max_cost) ectx ops
 *                output
 *            in
 *            new Synth.synthesizer ctx)
 *          params
 *          (Option.iter ~f:(fun p -> eprint_s [%message (p : Op.t Program.t)]))] *)
