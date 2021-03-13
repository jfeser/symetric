module type S = sig
  include Lattice_intf.S

  type conc [@@deriving sexp]

  val contains : t -> conc -> bool
end
