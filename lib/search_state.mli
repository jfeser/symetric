module Make (Lang : Lang_abs_intf.S) :
  Search_state_intf.S
    with type op = Lang.Op.t
     and type type_ = Lang.Type.t
     and type abs = Lang.Abs.t
     and type params = Lang.params
