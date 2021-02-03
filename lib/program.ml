open Ast

module T = struct
  type t = [ `Apply of Offset.t Op.t * t list ] [@@deriving compare, hash, sexp]
end

let rec ceval params (`Apply (op, args)) =
  Conc.eval params op (List.map args ~f:(ceval params))

let rec size (`Apply (_, args)) = 1 + List.sum (module Int) args ~f:size

let check params prog =
  [%test_result: Conc.t] ~message:"checking program failed"
    ~expect:(Conc.bool_vector params.Params.bench.Bench.output)
    (ceval params prog)

include T
include Comparator.Make (T)