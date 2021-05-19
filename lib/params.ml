module P = Dumb_params

type t = Dumb_params.t

let seed = P.int ~name:"seed" ~doc:" random seed" ~init:(`Cli (Some 0)) ()

let max_cost = P.int ~name:"max-cost" ~doc:" max search cost" ()

let print_csv =
  P.bool ~name:"print-csv" ~doc:" print csv when done" ~init:(`Cli (Some false))
    ()

let print_csv_header =
  P.bool ~name:"print-csv-header" ~doc:" print csv header and exit"
    ~init:(`Cli (Some false)) ()

let runtime = P.span_ref ~name:"runtime" ()

let spec =
  P.
    [
      to_spec seed;
      to_spec max_cost;
      to_spec print_csv;
      to_spec runtime;
      to_spec print_csv_header;
    ]

let get = Dumb_params.get
