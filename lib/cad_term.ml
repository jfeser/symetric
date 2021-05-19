module Op = Cad_op
module Type = Cad_type

module Value = Term_value.Make (struct
  type type_ = Type.t

  include Op
end)

module Bench = struct
  include Cad_bench

  let output = solution_exn
end

type bench = Cad_bench.t

open Params

let lang = P.const_str ~name:"lang" "cad"

include Cad_params

let spec = spec @ [ P.to_spec lang ]
