module Make (Lang : Lang_intf.S) : sig
  module Search_state : Search_state_intf.S

  val synth : Lang.Bench.t Params.t -> Search_state.t
end
