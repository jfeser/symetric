type t = { hi : Box.t; lo : Box.t } [@@deriving compare, hash, sexp]

type conc = Cad_conc.t

let is_subset x ~of_:y =
  Box.is_subset x.hi ~of_:y.hi && Box.is_subset y.lo ~of_:x.lo

let create ~lo ~hi =
  assert (Box.is_subset ~of_:hi lo);
  { hi; lo }

let top = create ~lo:Box.bot ~hi:Box.top

let bot = create ~lo:Box.bot ~hi:Box.bot

let lub a b = create ~lo:(Box.glb a.lo b.lo) ~hi:(Box.lub a.hi b.hi)

let glb a b = create ~lo:(Box.glb a.lo b.lo) ~hi:(Box.glb a.hi b.hi)

let contains a p = Set.for_all p ~f:(Box.contains a.hi)
