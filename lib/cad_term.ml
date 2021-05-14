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

type lparams = Cad_params.t

type params = (bench, lparams) Params.t
