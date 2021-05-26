module Op = Cad_op
module Type = Cad_type
module Symb = Cad_symb
module Value = Cad_conc
module Bench = Cad_bench

type symb = Cad_symb.t

type bench = Cad_bench.t

include struct
  open Dumb_params

  let spec = Spec.union [ Value.spec; Cad_params.spec ]

  let lang = Spec.add spec @@ Param.const_str ~name:"lang" "cad"
end

let bench = Cad_params.bench
