let fresh = Fresh.create ()

let enable_dump = ref false

let max_cost = ref 10

let enable_forced_bit_check = ref false

let n_bits : int Set_once.t = Set_once.create ()

let inputs : Vector3.t array Set_once.t = Set_once.create ()
