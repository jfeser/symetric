module Make (Op : Op_intf.S) = struct
  module T = struct
    type t = Op.t Program.t [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let eval _ a b = Program.Apply (a, b)
end
