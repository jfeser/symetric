module Op = Cad_op
module Type = Cad_type
module Symb = Cad_symb
module Value = Cad_conc
module Bench = Cad_bench

type symb = Cad_symb.t

type bench = Cad_bench.t

open Params

let lang = P.const_str ~name:"lang" "cad"

include Cad_params

let spec = spec @ [ P.to_spec lang ]
