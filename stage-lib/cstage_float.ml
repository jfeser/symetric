open! Core
open Types

module type S = sig
  type t
  type 'a code
  type 'a ctype

  val type_ : t ctype
  val float : float -> t code
  val ( ~- ) : t code -> t code
  val ( + ) : t code -> t code -> t code
  val ( - ) : t code -> t code -> t code
  val ( * ) : t code -> t code -> t code
  val ( / ) : t code -> t code -> t code
  val ( ** ) : t code -> t code -> t code
  val ( > ) : t code -> t code -> bool code
  val ( < ) : t code -> t code -> bool code
  val ( >= ) : t code -> t code -> bool code
  val ( <= ) : t code -> t code -> bool code
  val ( = ) : t code -> t code -> bool code
  val min : t code -> t code -> t code
  val max : t code -> t code -> t code
  val cos : t code -> t code
  val sin : t code -> t code
  val of_sexp : sexp code -> t code
  val sexp_of : t code -> sexp code
end

module Make (C : Cstage_core.S) = struct
  open C

  type t

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
  let of_sexp x = eformat "std::atof(((atom*)$(x))->get_body().c_str())" type_ "" [ ("x", C x) ]
  let sexp_of x = eformat "(new atom(std::to_string($(x))))" Sexp.type_ "" [ ("x", C x) ]
end
