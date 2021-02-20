module Make (Lang : Lang_intf.S) :
  Search_state_intf.S
    with type op = Lang.Op.t
     and type type_ = Lang.Type.t
     and type abs = Lang.Abs.t
     and type bench = Lang.Bench.t
