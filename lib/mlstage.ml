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
      | Sexp of Sexp.t
      | String of string
    [@@deriving compare, sexp_of]
  end

  open Value

  type 'a t = (unit -> Value.t[@opaque]) [@@deriving sexp_of]

  type ctype = unit [@@deriving compare, sexp]

  let let_ v f = f v

  let let_global = let_

  let genlet = Fun.id

  let let_locus f = f ()

  let to_int x = match x () with Int x -> x | _ -> assert false

  let to_bool x = match x () with Bool x -> x | _ -> assert false

  let to_unit x = match x () with Unit -> () | _ -> assert false

  let to_sexp x = match x () with Sexp x -> x | _ -> assert false

  let to_str x = match x () with String x -> x | _ -> assert false

  let to_value x = x ()

  let to_code x () = x

  let to_string x = to_value x |> [%sexp_of: Value.t] |> Sexp.to_string_hum

  let unit_t = ()

  let int_t = ()

  let bool_t = ()

  let type_of _ = ()

  let cast = Fun.id

  let unit () = Unit

  module Sexp = struct
    let type_ = ()

    let input () () = Sexp (Sexp.input_sexp In_channel.stdin)

    module List = struct
      let get s i () =
        match to_sexp s with
        | List l -> Sexp (List.nth_exn l (to_int i))
        | _ -> failwith "Expected a list"

      let length s () =
        match to_sexp s with
        | List l -> Int (List.length l)
        | _ -> failwith "Expected a list"
    end

    let to_list = Fun.id
  end

  module Int = struct
    let type_ = int_t

    let int x () = Int x

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

    let of_sexp x () = Int ([%of_sexp: int] (to_sexp x))

    let sexp_of x () = Sexp ([%sexp_of: int] (to_int x))
  end

  module Bool = struct
    let type_ = bool_t

    let bool x () = Bool x

    let bool_binop f x x' () = Bool (f (to_bool x) (to_bool x'))

    let ( && ) = bool_binop ( && )

    let ( || ) = bool_binop ( || )

    let not x () = Bool (not (to_bool x))

    let of_sexp x () = Bool ([%of_sexp: bool] (to_sexp x))

    let sexp_of x () = Sexp ([%sexp_of: bool] (to_bool x))
  end

  module String = struct
    let type_ = ()

    module O = struct
      let ( = ) x y () =
        Bool ([%compare.equal: Value.t] (to_value x) (to_value y))
    end

    let of_sexp x () =
      match to_sexp x with
      | Atom s -> String s
      | _ -> failwith "Expected an atom."

    let sexp_of x () = Sexp ([%sexp_of: string] (to_str x))

    let print s () =
      print_endline (to_str s);
      Unit

    let input () = String (In_channel.input_all In_channel.stdin)

    let const s () = String s
  end

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

    let iter a ~f () =
      to_array a |> Array.iter ~f:(fun x -> f (fun () -> x) |> to_unit);
      Unit

    let sub a i i' () =
      Array (to_array a |> Array.(sub ~pos:(to_int i) ~len:(to_int i')))

    let init _ i f () =
      Array (Array.init (to_int i) ~f:(fun i -> f Int.(int i) |> to_value))

    let map _ a ~f () =
      Array (Array.map (to_array a) ~f:(fun x -> f (to_code x) |> to_value))

    let map2 _ a a' ~f () =
      Array
        (Array.map2_exn (to_array a) (to_array a') ~f:(fun x x' ->
             f (to_code x) (to_code x') |> to_value))

    let of_sexp _ x elem_of_sexp () =
      match to_sexp x with
      | List ls ->
          Array
            ( List.map ls ~f:(fun s ->
                  elem_of_sexp (fun () -> Sexp s) |> to_value)
            |> Array.of_list )
      | _ -> failwith "Expected a list."

    let sexp_of x sexp_of_elem () =
      Sexp
        (Core.Sexp.List
           ( to_array x
           |> Array.map ~f:(fun e -> sexp_of_elem (to_code e) |> to_sexp)
           |> Array.to_list ))
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

    let of_sexp _ x elem_of_sexp () =
      match to_sexp x with
      | List ls ->
          Set
            ( List.map ls ~f:(fun s ->
                  elem_of_sexp (fun () -> Sexp s) |> to_value)
            |> Set.Poly.of_list |> ref )
      | _ -> failwith "Expected a list."

    let sexp_of x sexp_of_elem () =
      Sexp
        (Core.Sexp.List
           ( to_set x |> ( ! ) |> Set.Poly.to_list
           |> List.map ~f:(fun e -> sexp_of_elem (to_code e) |> to_sexp) ))
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

    let of_sexp x t1_of_sexp t2_of_sexp () =
      match to_sexp x with
      | List [ t1; t2 ] ->
          Tuple
            ( t1_of_sexp (fun () -> Sexp t1) |> to_value,
              t2_of_sexp (fun () -> Sexp t2) |> to_value )
      | _ -> failwith "Expected a list."

    let sexp_of x sexp_of_t1 sexp_of_t2 () =
      let t1, t2 = to_tuple x in
      Sexp
        (Core.Sexp.List
           [
             sexp_of_t1 (to_code t1) |> to_sexp;
             sexp_of_t2 (to_code t2) |> to_sexp;
           ])
  end

  let for_ l s h f () =
    let lo = to_int l in
    let step = to_int s in
    let hi = to_int h in
    let rec loop i =
      if Core.Int.(i >= hi) then Unit
      else (
        to_unit (f Int.(int i));
        loop Core.Int.(i + step) )
    in
    loop lo

  let ite c t e = if to_bool c then t () else e ()

  let seq x y () =
    to_unit x;
    to_value y

  let sseq = List.fold_left ~init:unit ~f:seq

  let exit () = raise Exit

  let print s () =
    print_endline s;
    Unit

  let to_func x = match to_value x with Func x -> x | _ -> assert false

  module Func = struct
    let mk_type _ _ = ()

    let func _ _ f () = Func (fun x -> f (to_code x) |> to_value)

    let apply f x () = (to_func f) (to_value x)
  end
end
