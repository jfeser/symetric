open! Core
open Staged_synth

let () = Signal.Expert.handle Signal.int (fun _ -> exit 1)

let print_header () =
  Fmt.pr
    "k,n,seed,max_cost,abstraction,n_state_nodes,n_arg_nodes,n_covered,n_refuted,min_width,max_width,median_width,check,sat\n"

let csg_cli =
  let module Csg = struct
    module Op = Csg_op
    module Type = Csg_type
    module Abs = Csg_abs
    module Symb = Csg_symb
    module Conc = Csg_conc
    module Bench = Csg_bench

    type symb = Csg_symb.t

    type bench = Csg_bench.t
  end in
  let module Lazy_cegis = Lazy_cegis.Make (Csg) in
  let csg_run params () =
    (Lazy_cegis.synth params : Lazy_cegis.Search_state.t) |> ignore
  in

  let open Command.Let_syntax in
  Command.basic ~summary:"Synthesize a CAD program using lazy cegis."
    [%map_open
      let params =
        Params.cli
          [%map_open
            let bench_fn = anon ("bench" %: string) in
            Sexp.load_sexp_conv_exn bench_fn [%of_sexp: Csg.Bench.t]]
      in
      csg_run params]

let () =
  Command.group ~summary:"Run lazy CEGIS."
    [
      ( "header",
        Command.basic ~summary:"Print stats header."
          (Command.Param.return print_header) );
      ("cad", csg_cli);
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
