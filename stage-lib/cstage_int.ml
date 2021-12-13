open! Core
open Types

module type S = sig
  type t
  type 'a code
  type 'a ctype

  val type_ : t ctype
  val int : Int.t -> t code
  val ( ~- ) : t code -> t code
  val ( + ) : t code -> t code -> t code
  val ( - ) : t code -> t code -> t code
  val ( * ) : t code -> t code -> t code
  val ( / ) : t code -> t code -> t code
  val ( mod ) : t code -> t code -> t code
  val ( > ) : t code -> t code -> bool code
  val ( < ) : t code -> t code -> bool code
  val ( >= ) : t code -> t code -> bool code
  val ( <= ) : t code -> t code -> bool code
  val ( = ) : t code -> t code -> bool code
  val min : t code -> t code -> t code
  val max : t code -> t code -> t code
  val of_sexp : sexp code -> t code
  val sexp_of : t code -> sexp code
end

module type S_untyped = sig
  type expr
  type ctype

  val type_ : ctype
  val int : int -> expr
  val ( ~- ) : expr -> expr
  val ( + ) : expr -> expr -> expr
  val ( - ) : expr -> expr -> expr
  val ( * ) : expr -> expr -> expr
  val ( / ) : expr -> expr -> expr
  val ( mod ) : expr -> expr -> expr
  val ( = ) : expr -> expr -> expr
  val ( > ) : expr -> expr -> expr
  val ( < ) : expr -> expr -> expr
  val ( <= ) : expr -> expr -> expr
  val ( >= ) : expr -> expr -> expr
  val min : expr -> expr -> expr
  val max : expr -> expr -> expr
  val of_sexp : expr -> expr
  val sexp_of : expr -> expr
end

module Int (C : Cstage_core.S) = struct
  open C

  type t = Types.int

  let type_ = Type.create ~name:"int32_t"
  let int x = eformat (sprintf "%d" x) type_ "" []
  let ( ~- ) x = unop "(-%s)" type_ x
  let ( + ) x y = binop "(%s + %s)" type_ x y
  let ( - ) x y = binop "(%s - %s)" type_ x y
  let ( * ) x y = binop "(%s * %s)" type_ x y
  let ( / ) x y = binop "(%s / %s)" type_ x y
  let ( mod ) x y = binop "(%s %% %s)" type_ x y
  let ( = ) x y = binop "(%s == %s)" Bool.type_ x y
  let ( > ) x y = binop "(%s > %s)" Bool.type_ x y
  let ( < ) x y = binop "(%s < %s)" Bool.type_ x y
  let ( <= ) x y = binop "(%s <= %s)" Bool.type_ x y
  let ( >= ) x y = binop "(%s >= %s)" Bool.type_ x y
  let min x y = binop "std::min(%s, %s)" type_ x y
  let max x y = binop "std::max(%s, %s)" type_ x y
  let of_sexp x = eformat "std::stoi(((atom*)$(x))->get_body())" type_ "" [ ("x", C x) ]
  let sexp_of x = eformat "(new atom(std::to_string($(x))))" Sexp.type_ "" [ ("x", C x) ]
end

module Int16 (C : Cstage_core.S) = struct
  include Int (C)

  let type_ = C.Type.create ~name:"int16_t"
end

module Int8 (C : Cstage_core.S) = struct
  include Int (C)

  let type_ = C.Type.create ~name:"int8_t"
end
