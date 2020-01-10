open! Core

module Code : Sigs.CODE = struct
  exception Exit

  module Value = struct
    type t =
      | Unit
      | Int of int
      | Bool of bool
      | Array of t array
      | Tuple of (t * t)
      | Set of t Set.Poly.t ref
      | Func of ((t -> t)[@compare.ignore])
    [@@deriving compare, sexp_of]
  end

  open Value

  type 'a set

  type 'a t = (unit -> Value.t[@opaque]) [@@deriving sexp_of]

  type ctype = unit [@@deriving compare, sexp]

  let let_ v f = f v

  let let_global = let_

  let genlet = Fun.id

  let let_locus f = f ()

  let to_string _ = failwith "Undefined"

  let to_int x = match x () with Int x -> x | _ -> assert false

  let to_bool x = match x () with Bool x -> x | _ -> assert false

  let to_unit x = match x () with Unit -> () | _ -> assert false

  let to_value x = x ()

  let to_code x () = x

  let unit_t = ()

  let int_t = ()

  let bool_t = ()

  let type_of _ = ()

  let cast = Fun.id

  let unit () = Unit

  let int x () = Int x

  let bool x () = Bool x

  let ( ~- ) x () = Int (-to_int x)

  let int_binop f x x' () = Int (f (to_int x) (to_int x'))

  let ( + ) = int_binop ( + )

  let ( - ) = int_binop ( - )

  let ( * ) = int_binop ( * )

  let ( / ) = int_binop ( / )

  let ( mod ) = int_binop ( mod )

  let min = int_binop Int.min

  let max = int_binop Int.max

  let cmp_binop f x x' () = Bool (f (to_int x) (to_int x'))

  let ( > ) = cmp_binop ( > )

  let ( >= ) = cmp_binop ( >= )

  let ( < ) = cmp_binop ( < )

  let ( <= ) = cmp_binop ( <= )

  let ( = ) = cmp_binop ( = )

  let bool_binop f x x' () = Bool (f (to_bool x) (to_bool x'))

  let ( && ) = bool_binop ( && )

  let ( || ) = bool_binop ( || )

  let not x () = Bool (not (to_bool x))

  module Array = struct
    let mk_type _ = ()

    let elem_type _ = ()

    module O = struct
      let ( = ) x y () =
        Bool ([%compare.equal: Value.t] (to_value x) (to_value y))
    end

    let to_array x = match x () with Array x -> x | _ -> assert false

    let const _ a () = Array (Array.map a ~f:to_value)

    let get a i () = (to_array a).(to_int i)

    let set a i x () =
      (to_array a).(to_int i) <- to_value x;
      Unit

    let length a () = Int (to_array a |> Array.length)

    let fold a ~init ~f () =
      to_array a
      |> Array.fold ~init ~f:(fun acc x -> f acc (fun () -> x))
      |> to_value

    let sub a i i' () =
      Array (to_array a |> Array.(sub ~pos:(to_int i) ~len:(to_int i')))

    let init _ i f () =
      Array (Array.init (to_int i) ~f:(fun i -> f (int i) |> to_value))

    let map _ a ~f () =
      Array (Array.map (to_array a) ~f:(fun x -> f (to_code x) |> to_value))

    let map2 _ a a' ~f () =
      Array
        (Array.map2_exn (to_array a) (to_array a') ~f:(fun x x' ->
             f (to_code x) (to_code x') |> to_value))
  end

  module Set = struct
    let mk_type _ = ()

    let to_set x = match to_value x with Set x -> x | _ -> assert false

    let empty _ () = Set (ref Set.Poly.empty)

    let add s x () =
      let s = to_set s in
      s := Set.add !s (to_value x);
      Unit

    let iter s f () =
      Set.iter !(to_set s) ~f:(fun x -> to_unit (f (to_code x)));
      Unit

    let fold s ~init ~f () =
      Set.fold !(to_set s) ~init ~f:(fun acc x -> f acc (to_code x)) |> to_value
  end

  module Tuple = struct
    let mk_type _ _ = ()

    let to_tuple x = match to_value x with Tuple x -> x | _ -> assert false

    let create x y () = Tuple (to_value x, to_value y)

    let fst x () =
      let x, _ = to_tuple x in
      x

    let snd x () =
      let _, x = to_tuple x in
      x
  end

  let for_ l s h f () =
    let lo = to_int l in
    let step = to_int s in
    let hi = to_int h in
    let rec loop i =
      if Int.(i >= hi) then Unit
      else (
        to_unit (f (int i));
        loop Int.(i + step) )
    in
    loop lo

  let ite c t e = if to_bool c then t () else e ()

  let seq x y () =
    to_unit x;
    to_value y

  let seq_many = List.fold_left ~init:unit ~f:seq

  let exit () = raise Exit

  let print s () =
    print_endline s;
    Unit

  let to_func x = match to_value x with Func x -> x | _ -> assert false

  let func _ _ f () = Func (fun x -> f (to_code x) |> to_value)

  let func_t _ _ = ()

  let apply f x () = (to_func f) (to_value x)
end
