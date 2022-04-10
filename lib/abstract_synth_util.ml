open Std

module type Value_intf = sig
  type t [@@deriving compare, equal, hash, sexp]

  module Ctx : sig
    type t
  end

  include Comparator.S with type t := t

  val is_error : t -> bool
end

module type Domain_atomic_intf = sig
  type concrete
  type op
  type ctx
  type t [@@deriving compare, hash, sexp]

  val cost : [ `Concrete of concrete | `Pred of t ] -> int
  val lift : concrete -> t Iter.t
  val complete : t -> t Iter.t
  val eval : t -> concrete -> bool
end

module Predicate_domain (Value : Value_intf) (Domain_pred : Domain_atomic_intf with type concrete := Value.t) = struct
  (** Atomic predicates are either domain specific atomic predicates or concrete values. *)
  module Atomic = struct
    module T = struct
      type t = [ `Concrete of Value.t | `Pred of Domain_pred.t ] [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)

    let cost = Domain_pred.cost
  end

  type t = Bottom | Preds of Set.M(Atomic).t [@@deriving compare, equal, hash, sexp]

  let of_iter iter =
    Iter.fold
      (fun ps p ->
        match (ps, p) with
        | ps, `True -> ps
        | _, `False | Bottom, _ -> Bottom
        | Preds pp, ((`Pred _ | `Concrete _) as p) -> Preds (Set.add pp p))
      (Preds (Set.empty (module Atomic)))
      iter

  let to_iter = function
    | Bottom -> Iter.empty
    | Preds ps ->
        Iter.cons `True @@ Iter.map (function #Atomic.t as p -> p) @@ (Iter.of_set ps :> [> Atomic.t ] Iter.t)

  let lift conc =
    let domain = Domain_pred.lift conc |> Iter.map (fun p -> `Pred p) in
    of_iter (Iter.cons (`Concrete conc) domain)

  let and_ p p' = match (p, p') with Bottom, _ | _, Bottom -> Bottom | Preds ps, Preds ps' -> Preds (Set.union ps ps')
  let and_many = Iter.fold and_ (Preds (Set.empty (module Atomic)))

  let complete = function
    | Bottom as p -> p
    | Preds ps ->
        Iter.of_set ps
        |> Iter.map (function
             | `Concrete v as p -> and_ (Preds (Set.singleton (module Atomic) p)) @@ lift v
             | `Pred p -> Domain_pred.complete p |> Iter.map (fun p -> `Pred p) |> of_iter)
        |> and_many

  let implies p p' =
    let p = complete p and p' = complete p' in
    match (p, p') with Bottom, _ -> true | _, Bottom -> false | Preds ps, Preds ps' -> Set.is_subset ps' ~of_:ps

  let contains p v =
    match p with
    | Bottom -> false
    | Preds ps ->
        Set.for_all ps ~f:(function `Pred p -> Domain_pred.eval p v | `Concrete v' -> [%compare.equal: Value.t] v v')

  let length = function Bottom -> 0 | Preds ps -> Set.length ps
  let singleton p = of_iter @@ Iter.singleton p
end
