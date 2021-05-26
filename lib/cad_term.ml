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

include Cad_params

let lang =
  let module P = Dumb_params in
  P.Spec.add spec @@ P.Param.const_str ~name:"lang" "cad"
