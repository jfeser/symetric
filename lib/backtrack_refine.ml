module Make
    (Lang : Lang_intf.S)
    (Search_state : Search_state_intf.S
                      with type op = Lang.Op.t
                       and type abs = Lang.Abs.t
                       and type bench = Lang.Bench.t) =
struct
  open Lang
  open Search_state

  module Refinement = struct
    type s = { old : Set.M(Abs).t; new_ : Set.M(Abs).t } [@@deriving sexp]

    type t = s Map.M(Search_state.Args).t [@@deriving sexp_of]
  end

  let refine ss target =
    raise_s [%message (List.map target ~f:(State.to_message ss) : Sexp.t list)]
end
