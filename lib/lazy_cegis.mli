module Make
    (Lang : Lang_intf.S)
    (Search_state : Search_state_intf.S
                      with type op = Lang.Op.t
                       and type abs = Lang.Abs.t
                       and type bench = Lang.Bench.t
                       and type type_ = Lang.Type.t)
    (Refine : Refine_intf.S
                with type op := Lang.Op.t
                 and module Search_state := Search_state
                 and module Abs := Lang.Abs) : sig
  val synth : Lang.Bench.t Params.t -> Search_state.t
end
