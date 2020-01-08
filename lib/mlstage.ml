open! Core

module Code = struct
  type value =
    | Unit
    | Int of int
    | Bool of bool
    | Array of value array
    | Tuple of (value * value)
  [@@deriving sexp_of]

  type 'a t = value [@@deriving sexp_of]

  type ctype = unit [@@deriving compare, sexp]

  let to_int = function Int x -> x | _ -> assert false

  let to_bool = function Bool x -> x | _ -> assert false

  let unit = Unit

  let int x = Int x

  let bool x = Bool x

  let ( ~- ) x = Int (-to_int x)

  let int_binop f x x' = Int (f (to_int x) (to_int x'))

  let ( + ) = int_binop ( + )

  let ( - ) = int_binop ( - )

  let ( * ) = int_binop ( * )

  let ( / ) = int_binop ( / )

  let ( mod ) = int_binop ( mod )

  let min = int_binop Int.min

  let max = int_binop Int.max

  let cmp_binop f x x' = Bool (f (to_int x) (to_int x'))

  let ( > ) = cmp_binop ( > )

  let ( >= ) = cmp_binop ( >= )

  let ( < ) = cmp_binop ( < )

  let ( <= ) = cmp_binop ( <= )

  let ( = ) = cmp_binop ( = )

  let bool_binop f x x' = Bool (f (to_bool x) (to_bool x'))

  let ( && ) = bool_binop ( && )

  let ( || ) = bool_binop ( || )

  let not x = Bool (not (to_bool x))

  module Array = struct end
end
