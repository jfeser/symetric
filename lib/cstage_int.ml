open! Core

module type S = sig
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

  let type_ = Type.create ~name:"int"

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

  let of_sexp x =
    eformat "std::stoi(((atom*)$(x).get())->get_body())" type_ "" [ ("x", C x) ]

  let sexp_of x =
    eformat "atom(std::to_string($(x)))" Sexp.type_ "" [ ("x", C x) ]
end
