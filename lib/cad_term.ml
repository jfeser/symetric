module Op = Cad_op
module Type = Cad_type

module Value = struct
  include Term_value.Make (Op)
  module Ctx = Cad.Value.Ctx

  let dist ctx p p' =
    let eval = Program.eval (Cad.Value.eval ctx) in
    let v = eval p and v' = eval p' in
    Float.of_int @@ Cad_conc.hamming v v'

  let embed _ = failwith "unimplemented"
end

module Bench = struct
  include Cad_bench

  let output = solution_exn
end

type bench = Cad_bench.t

include Cad_params

let name = "cad-term"

let lang =
  let module P = Dumb_params in
  P.Spec.add spec @@ P.Param.const_str ~name:"lang" name
