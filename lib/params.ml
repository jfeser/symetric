type 'b t = {
  fresh : Fresh.t; [@ignore]
  enable_dump : bool;
  max_cost : int;
  enable_forced_bit_check : bool;
  hide_values : bool;
  bench : 'b;
  validate : bool;
  state_set : [ `Full | `Roots ];
  cone : [ `Full | `Rand ];
}
[@@deriving sexp]

let cli bench =
  let open Command.Let_syntax in
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
    and bench = bench in

    {
      bench;
      fresh = Fresh.create ();
      enable_dump;
      max_cost;
      enable_forced_bit_check;
      hide_values;
      validate = false;
      state_set = `Full;
      cone = `Full;
    }]
