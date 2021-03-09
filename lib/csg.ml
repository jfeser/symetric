module Op = Csg_op
module Type = Csg_type
module Abs = Csg_abs
module Symb = Csg_symb
module Conc = Csg_conc
module Bench = Csg_bench

type symb = Csg_symb.t

type bench = Csg_bench.t

type lparams = unit

type params = (bench, lparams) Params.t
