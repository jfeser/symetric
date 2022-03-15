module S :
  Search_state_intf.S
    with type type_ := Cad_ext.Type.t
     and type value := Cad_ext.Value.t
     and type op := Cad_ext.Op.t

val cmd : Command.t
