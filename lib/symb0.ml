module Bool_vector = struct
  type elem = Free of Smt.Var.t | Fixed of bool [@@deriving compare, sexp]

  type t = elem list [@@deriving sexp]

  let free x = Free x

  let fixed x = Fixed x

  let to_expr = function Free v -> Smt.var v | Fixed x -> Smt.bool x
end

module Offset = struct
  type t = { set : Bool_vector.t; type_ : Offset_type.t } [@@deriving sexp]
end

type t = Bool_vector of Bool_vector.t | Offset of Offset.t [@@deriving sexp]
