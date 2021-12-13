open Base_quickcheck

module Make (L : Lattice_intf.S) = struct
  module L2 = struct
    type t = L.t * L.t [@@deriving compare, sexp, quickcheck]
  end

  let quickcheck_generator_leq_default _ = Generator.return L.bot
  let quickcheck_generator_leq = Option.value L.quickcheck_generator_leq ~default:quickcheck_generator_leq_default

  module Leq2 = struct
    type t = L.t * L.t [@@deriving compare, sexp, quickcheck]

    let quickcheck_generator =
      let open Generator.Let_syntax in
      let%bind b = [%quickcheck.generator: L.t] in
      let%bind a = quickcheck_generator_leq b in
      return (a, b)
  end

  module Leq3 = struct
    type t = L.t * L.t * L.t [@@deriving compare, sexp, quickcheck]

    let quickcheck_generator =
      let open Generator.Let_syntax in
      let%bind c = [%quickcheck.generator: L.t] in
      let%bind b = quickcheck_generator_leq c in
      let%bind a = quickcheck_generator_leq b in
      return (a, b, c)
  end

  module L3 = struct
    type t = L.t * L.t * L.t [@@deriving compare, sexp, quickcheck]
  end
end
