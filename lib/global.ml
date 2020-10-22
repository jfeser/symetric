open Ast

let fresh = Fresh.create ()

let enable_dump = ref false

let max_cost = ref 10

let enable_forced_bit_check = ref false

let bench : Bench.t Set_once.t = Set_once.create ()

let n_bits = lazy (Array.length (Set_once.get_exn bench [%here]).input)
