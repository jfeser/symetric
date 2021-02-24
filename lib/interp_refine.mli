module Make
    (Lang : Lang_intf.S)
    (Search_state : Search_state_intf.S
                      with type op = Lang.Op.t
                       and type abs = Lang.Abs.t
                       and type bench = Lang.Bench.t) :
  Refine_intf.S
    with type op := Lang.Op.t
     and module Search_state := Search_state
     and module Abs := Lang.Abs
