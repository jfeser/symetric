module Op = Cad_op
module Type = Cad_type
module Abs = Cad_conc
module Symb = Cad_symb
module Conc = Cad_conc
module Bench = Cad_bench

type symb = Cad_symb.t

type bench = Cad_bench.t

type lparams = Cad_params.t

type params = (bench, lparams) Params.t
