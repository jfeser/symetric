module M (K : sig
  type t

  type comparator_witness
end) =
struct
  type 'a t = ('a * Set.M(K).t * Set.M(K).t) list
end
