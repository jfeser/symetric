type t = {
  fresh : Fresh.t;
  enable_dump : bool;
  max_cost : int;
  enable_forced_bit_check : bool;
  hide_values : bool;
  bench : Offset.t Bench.t;
  n_bits : int;
  offsets : Offset.ctx;
  validate : bool;
  state_set : [ `Full | `Roots ];
  cone : [ `Full | `Rand ];
}

let create ?(enable_dump = false) ?(max_cost = 10)
    ?(enable_forced_bit_check = false) ?(hide_values = false) bench =
  let bench, offsets = Offset.of_bench bench in
  {
    fresh = Fresh.create ();
    enable_dump;
    max_cost;
    enable_forced_bit_check;
    hide_values;
    bench;
    n_bits = Array.length bench.input;
    offsets;
    validate = false;
    state_set = `Full;
    cone = `Full;
  }

let bool_vector_abs = `Precise