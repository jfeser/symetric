module Make
    (Lang : Lang_intf.S)
    (Search_state : Search_state_intf.S
                      with type op = Lang.Op.t
                       and type abs = Lang.Abs.t
                       and type bench = Lang.Bench.t) : sig
  open Lang

  module Refinement : sig
    type s = { old : Set.M(Abs).t; new_ : Set.M(Abs).t } [@@deriving sexp]

    type t = s Map.M(Search_state.Args).t [@@deriving sexp_of]
  end

  val refine :
    Search_state.t ->
    Search_state.State.t list ->
    (Refinement.t, Op.t Program.t) Either.t
end
