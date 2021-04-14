module type S = sig
  module L : Lattice_intf.S

  module M : Lattice_intf.S

  val lift : L.t -> M.t

  val lower : M.t -> L.t
end
