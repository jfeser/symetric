type ('a, 'b) t = {
  random_state : (Random.State.t[@sexp.opaque]);
  fresh : (Fresh.t[@sexp.opaque]);
  enable_dump : bool;
  max_cost : int;
  enable_forced_bit_check : bool;
  hide_values : bool;
  bench : 'a;
  lparams : 'b;
  validate : bool;
  state_set : [ `Full | `Roots ];
  cone : [ `Full | `Rand ];
}
[@@deriving sexp_of]

let default_max_cost = 20

let create ?(enable_dump = false) ?(max_cost = default_max_cost)
    ?(enable_forced_bit_check = false) ?(hide_values = false)
    ?(validate = false) ?(state_set = `Full) ?(cone = `Full) ?(seed = 0) bench
    lparams =
  {
    bench;
    lparams;
    fresh = Fresh.create ();
    random_state = Random.State.make [| seed |];
    enable_dump;
    max_cost;
    enable_forced_bit_check;
    hide_values;
    validate;
    state_set;
    cone;
  }

let cli bench lparams =
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
        (optional_with_default default_max_cost int)
        ~doc:" maximum program cost"
    and bench = bench
    and lparams = lparams in

    create ~enable_dump ~hide_values ~enable_forced_bit_check ~max_cost bench
      lparams]
