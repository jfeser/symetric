open! Core

module type S = sig
  type 'a t

  type 'a ctype

  type sexp

  val type_ : float ctype

  val float : float -> float t

  val ( ~- ) : float t -> float t

  val ( + ) : float t -> float t -> float t

  val ( - ) : float t -> float t -> float t

  val ( * ) : float t -> float t -> float t

  val ( / ) : float t -> float t -> float t

  val ( ** ) : float t -> float t -> float t

  val ( > ) : float t -> float t -> bool t

  val ( < ) : float t -> float t -> bool t

  val ( >= ) : float t -> float t -> bool t

  val ( <= ) : float t -> float t -> bool t

  val ( = ) : float t -> float t -> bool t

  val min : float t -> float t -> float t

  val max : float t -> float t -> float t

  val cos : float t -> float t

  val sin : float t -> float t

  val of_sexp : sexp t -> float t

  val sexp_of : float t -> sexp t
end

module Make (C : Cstage_core.S) = struct
  open C

  let type_ = Type.create ~name:"float"

  let float x = eformat (sprintf "%f" x) type_ "" []

  let ( ~- ) x = unop "(-%s)" type_ x

  let ( + ) x y = binop "(%s + %s)" type_ x y

  let ( - ) x y = binop "(%s - %s)" type_ x y

  let ( * ) x y = binop "(%s * %s)" type_ x y

  let ( / ) x y = binop "(%s / %s)" type_ x y

  let ( ** ) x y = binop "std::pow(%s, %s)" type_ x y

  let ( = ) x y = binop "(%s == %s)" Bool.type_ x y

  let ( > ) x y = binop "(%s > %s)" Bool.type_ x y

  let ( < ) x y = binop "(%s < %s)" Bool.type_ x y

  let ( <= ) x y = binop "(%s <= %s)" Bool.type_ x y

  let ( >= ) x y = binop "(%s >= %s)" Bool.type_ x y

  let min x y = binop "std::min(%s, %s)" type_ x y

  let max x y = binop "std::max(%s, %s)" type_ x y

  let cos x = unop "std::cos(%s)" type_ x

  let sin x = unop "std::sin(%s)" type_ x

  let of_sexp x =
    eformat "std::atof(((atom*)$(x))->get_body())" type_ "" [ ("x", C x) ]

  let sexp_of x =
    eformat "atom(std::to_string($(x)))" Sexp.type_ "" [ ("x", C x) ]
end
