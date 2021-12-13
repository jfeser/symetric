open! Core

module Code = struct
  exception Exit

  module Value = struct
    type value =
      | Unit
      | Int of int
      | Bool of bool
      | Array of t array
      | Tuple of (t * t)
      | Tuple_3 of (t * t * t)
      | Set of t Set.Poly.t ref
      | Func of ((t -> t)[@compare.ignore])
      | Sexp of Sexp.t
      | String of string
      | Float of float
      | Tuple_4 of (t * t * t * t)

    and t = { value : value; annot : Univ_map.t [@compare.ignore] } [@@deriving compare, sexp_of]
  end

  open Value

  type _ t = Value.t Lazy.t [@@deriving sexp_of]
  type _ ctype = unit [@@deriving compare, sexp]

  let let_ v f = f v
  let let_global = let_
  let genlet = Fun.id
  let let_locus f = f ()
  let to_value x = (Lazy.force x).value
  let to_value' x = Lazy.force x
  let to_int x = match to_value x with Int x -> x | _ -> assert false
  let to_float x = match to_value x with Float x -> x | _ -> assert false
  let to_bool x = match to_value x with Bool x -> x | _ -> assert false
  let to_unit x = match to_value x with Unit -> () | _ -> assert false
  let to_sexp x = match to_value x with Sexp x -> x | _ -> assert false
  let to_str x = match to_value x with String x -> x | _ -> assert false
  let to_array x = match to_value x with Array x -> x | _ -> assert false
  let to_set x = match to_value x with Set x -> x | _ -> assert false
  let to_code value = lazy value
  let to_string x = to_value x |> [%sexp_of: Value.value] |> Sexp.to_string_hum
  let unit_t = ()
  let int_t = ()
  let bool_t = ()
  let type_of _ = ()
  let cast x = (x : 'a t :> 'b t)
  let wrap thunk = lazy { value = thunk (); annot = Univ_map.empty }
  let wrap' = Lazy.from_fun
  let unit = wrap @@ fun () -> Unit

  module Sexp = struct
    let type_ = ()
    let sexp s = wrap @@ fun () -> Sexp s
    let input () = wrap @@ fun () -> Sexp (Sexp.input_sexp In_channel.stdin)

    let print x =
      wrap @@ fun () ->
      print_s @@ to_sexp x;
      Unit

    module List = struct
      let get s i =
        wrap @@ fun () ->
        match to_sexp s with List l -> Sexp (List.nth_exn l (to_int i)) | _ -> failwith "Expected a list"

      let length s =
        wrap @@ fun () -> match to_sexp s with List l -> Int (List.length l) | _ -> failwith "Expected a list"
    end

    let to_list x = (x : Types.sexp t :> Types.sexp list t)
  end

  module Int = struct
    type t = Types.int

    let type_ = int_t
    let int x = wrap @@ fun () -> Int x
    let ( ~- ) x = wrap @@ fun () -> Int (-to_int x)
    let int_binop f x x' = wrap @@ fun () -> Int (f (to_int x) (to_int x'))
    let ( + ) x y = int_binop ( + ) x y
    let ( - ) x y = int_binop ( - ) x y
    let ( * ) x y = int_binop ( * ) x y
    let ( / ) x y = int_binop ( / ) x y
    let ( mod ) x y = int_binop ( mod ) x y
    let min x y = int_binop Int.min x y
    let max x y = int_binop Int.max x y
    let cmp_binop f x x' = wrap @@ fun () -> Bool (f (to_int x) (to_int x'))
    let ( > ) x y = cmp_binop ( > ) x y
    let ( >= ) x y = cmp_binop ( >= ) x y
    let ( < ) x y = cmp_binop ( < ) x y
    let ( <= ) x y = cmp_binop ( <= ) x y
    let ( = ) x y = cmp_binop ( = ) x y
    let of_sexp x = wrap @@ fun () -> Int ([%of_sexp: int] (to_sexp x))
    let sexp_of x = wrap @@ fun () -> Sexp ([%sexp_of: int] (to_int x))
  end

  module Bool = struct
    type t

    let type_ = bool_t
    let bool x = wrap @@ fun () -> Bool x
    let bool_binop f x x' = wrap @@ fun () -> Bool (f (to_bool x) (to_bool x'))
    let ( && ) x y = bool_binop ( && ) x y
    let ( || ) x y = bool_binop ( || ) x y
    let not x = wrap @@ fun () -> Bool (not (to_bool x))
    let of_sexp x = wrap @@ fun () -> Bool ([%of_sexp: int] (to_sexp x) = 1)
    let sexp_of x = wrap @@ fun () -> Sexp ([%sexp_of: bool] (to_bool x))
  end

  module String = struct
    type t

    let type_ = ()

    module O = struct
      let ( = ) x y = wrap @@ fun () -> Bool ([%equal: string] (to_string x) (to_string y))
    end

    let of_sexp x = wrap @@ fun () -> match to_sexp x with Atom s -> String s | _ -> failwith "Expected an atom."
    let sexp_of x = wrap @@ fun () -> Sexp ([%sexp_of: string] (to_str x))

    let print s =
      wrap @@ fun () ->
      print_endline (to_str s);
      Unit

    let input = wrap @@ fun () -> String (In_channel.input_all In_channel.stdin)
    let const s = wrap @@ fun () -> String s
  end

  module Array = struct
    type nonrec 'a code = 'a t
    type nonrec 'a ctype = 'a ctype
    type 'a t = Array_t

    let mk_type _ = ()
    let elem_type _ = ()

    module O = struct
      let ( = ) x y = wrap @@ fun () -> Bool ([%compare.equal: Value.value] (to_value x) (to_value y))
    end

    let const _ a = wrap @@ fun () -> Array (Array.map a ~f:to_value')
    let get a i = wrap' @@ fun () -> (to_array a).(to_int i)

    let set a i x =
      wrap @@ fun () ->
      (to_array a).(to_int i) <- to_value' x;
      Unit

    let length a = wrap @@ fun () -> Int (to_array a |> Array.length)

    let fold a ~init ~f =
      wrap @@ fun () -> to_array a |> Array.fold ~init ~f:(fun acc x -> f acc @@ to_code x) |> to_value

    let iter a ~f =
      wrap @@ fun () ->
      to_array a |> Array.iter ~f:(fun x -> f @@ to_code x |> to_unit);
      Unit

    let sub a i i' = wrap @@ fun () -> Array (to_array a |> Array.(sub ~pos:(to_int i) ~len:(to_int i')))
    let init i f = wrap @@ fun () -> Array (Array.init (to_int i) ~f:(fun i -> f Int.(int i) |> to_value'))
    let map a ~f = wrap @@ fun () -> Array (Array.map (to_array a) ~f:(fun x -> f @@ to_code x |> to_value'))

    let map2 a a' ~f =
      wrap @@ fun () ->
      Array (Array.map2_exn (to_array a) (to_array a') ~f:(fun x x' -> f (to_code x) (to_code x') |> to_value'))

    let of_sexp x elem_of_sexp =
      wrap @@ fun () ->
      match to_sexp x with
      | List ls -> Array (List.map ls ~f:(fun s -> elem_of_sexp @@ Sexp.sexp s |> to_value') |> Array.of_list)
      | _ -> failwith "Expected a list."

    let sexp_of x sexp_of_elem =
      wrap @@ fun () ->
      Sexp (Core.Sexp.List (to_array x |> Array.map ~f:(fun e -> sexp_of_elem (to_code e) |> to_sexp) |> Array.to_list))
  end

  module Set = struct
    type 'a t = 'a Types.set

    let mk_type _ = ()
    let empty _ = wrap @@ fun () -> Set (ref Set.Poly.empty)

    let add s x =
      wrap @@ fun () ->
      let s = to_set s in
      s := Set.add !s (to_value' x);
      Unit

    let iter s f =
      wrap @@ fun () ->
      Set.iter !(to_set s) ~f:(fun x -> to_unit (f (to_code x)));
      Unit

    let fold s ~init ~f = wrap @@ fun () -> Set.fold !(to_set s) ~init ~f:(fun acc x -> f acc (to_code x)) |> to_value

    let of_sexp _ x elem_of_sexp =
      wrap @@ fun () ->
      match to_sexp x with
      | List ls -> Set (List.map ls ~f:(fun s -> elem_of_sexp @@ Sexp.sexp s |> to_value') |> Set.Poly.of_list |> ref)
      | _ -> failwith "Expected a list."

    let sexp_of x sexp_of_elem =
      wrap @@ fun () ->
      Sexp
        (Core.Sexp.List
           (to_set x |> ( ! ) |> Set.Poly.to_list |> List.map ~f:(fun e -> sexp_of_elem (to_code e) |> to_sexp)))
  end

  module Tuple = struct
    let mk_type _ _ = ()
    let to_tuple x = match to_value x with Tuple x -> x | _ -> assert false
    let create x y = wrap @@ fun () -> Tuple (to_value' x, to_value' y)

    let fst x =
      wrap' @@ fun () ->
      let x, _ = to_tuple x in
      x

    let snd x =
      wrap' @@ fun () ->
      let _, x = to_tuple x in
      x

    let of_sexp x t1_of_sexp t2_of_sexp =
      wrap @@ fun () ->
      match to_sexp x with
      | List [ t1; t2 ] -> Tuple (t1_of_sexp @@ Sexp.sexp t1 |> to_value', t2_of_sexp @@ Sexp.sexp t2 |> to_value')
      | _ -> failwith "Expected a list."

    let sexp_of x sexp_of_t1 sexp_of_t2 =
      wrap @@ fun () ->
      let t1, t2 = to_tuple x in
      Sexp (Core.Sexp.List [ sexp_of_t1 (to_code t1) |> to_sexp; sexp_of_t2 (to_code t2) |> to_sexp ])
  end

  module Tuple_3 = struct
    type (_, _, _) t

    let mk_type _ _ _ = ()
    let to_tuple x = match to_value x with Tuple_3 x -> x | _ -> assert false
    let create x y z = wrap @@ fun () -> Tuple_3 (to_value' x, to_value' y, to_value' z)

    let fst x =
      wrap' @@ fun () ->
      let x, _, _ = to_tuple x in
      x

    let snd x =
      wrap' @@ fun () ->
      let _, x, _ = to_tuple x in
      x

    let thd x =
      wrap' @@ fun () ->
      let _, _, x = to_tuple x in
      x

    let tuple_of x f = f (fst x, snd x, thd x)
    let of_tuple (x, y, z) = create x y z

    let of_sexp x t1_of_sexp t2_of_sexp t3_of_sexp =
      wrap @@ fun () ->
      match to_sexp x with
      | List [ t1; t2; t3 ] ->
          Tuple_3
            ( t1_of_sexp @@ Sexp.sexp t1 |> to_value',
              t2_of_sexp @@ Sexp.sexp t2 |> to_value',
              t3_of_sexp @@ Sexp.sexp t3 |> to_value' )
      | _ -> failwith "Expected a list."

    let sexp_of x sexp_of_t1 sexp_of_t2 sexp_of_t3 =
      wrap @@ fun () ->
      let t1, t2, t3 = to_tuple x in
      Sexp
        (Core.Sexp.List
           [
             sexp_of_t1 (to_code t1) |> to_sexp; sexp_of_t2 (to_code t2) |> to_sexp; sexp_of_t3 (to_code t3) |> to_sexp;
           ])
  end

  module Tuple_4 = struct
    type (_, _, _, _) t

    let mk_type _ _ _ _ = ()
    let to_tuple x = match to_value x with Tuple_4 x -> x | _ -> assert false
    let create x y z a = wrap @@ fun () -> Tuple_4 (to_value' x, to_value' y, to_value' z, to_value' a)

    let fst x =
      wrap' @@ fun () ->
      let x, _, _, _ = to_tuple x in
      x

    let snd x =
      wrap' @@ fun () ->
      let _, x, _, _ = to_tuple x in
      x

    let thd x =
      wrap' @@ fun () ->
      let _, _, x, _ = to_tuple x in
      x

    let fth x =
      wrap' @@ fun () ->
      let _, _, _, x = to_tuple x in
      x

    let tuple_of x f = f (fst x, snd x, thd x, fth x)
    let of_tuple (x, y, z, a) = create x y z a

    let of_sexp x t1_of_sexp t2_of_sexp t3_of_sexp t4_of_sexp =
      wrap @@ fun () ->
      match to_sexp x with
      | List [ t1; t2; t3; t4 ] ->
          Tuple_4
            ( t1_of_sexp @@ Sexp.sexp t1 |> to_value',
              t2_of_sexp @@ Sexp.sexp t2 |> to_value',
              t3_of_sexp @@ Sexp.sexp t3 |> to_value',
              t4_of_sexp @@ Sexp.sexp t4 |> to_value' )
      | _ -> failwith "Expected a list."

    let sexp_of x sexp_of_t1 sexp_of_t2 sexp_of_t3 sexp_of_t4 =
      wrap @@ fun () ->
      let t1, t2, t3, t4 = to_tuple x in
      Sexp
        (Core.Sexp.List
           [
             sexp_of_t1 (to_code t1) |> to_sexp;
             sexp_of_t2 (to_code t2) |> to_sexp;
             sexp_of_t3 (to_code t3) |> to_sexp;
             sexp_of_t4 (to_code t4) |> to_sexp;
           ])
  end

  module Float = struct
    type t = float

    let type_ = ()
    let float x = wrap @@ fun () -> Float x
    let float_unop f x = wrap @@ fun () -> Float (f (to_float x))
    let float_binop f x x' = wrap @@ fun () -> Float (f (to_float x) (to_float x'))
    let cmp_binop f x x' = wrap @@ fun () -> Bool (f (to_float x) (to_float x'))
    let of_sexp x = wrap @@ fun () -> Float ([%of_sexp: float] (to_sexp x))
    let sexp_of x = wrap @@ fun () -> Sexp ([%sexp_of: float] (to_float x))

    open Float

    let ( ~- ) x = float_unop ( ~- ) x
    let sin x = float_unop sin x
    let cos x = float_unop cos x
    let ( + ) x y = float_binop ( + ) x y
    let ( - ) x y = float_binop ( - ) x y
    let ( * ) x y = float_binop ( * ) x y
    let ( / ) x y = float_binop ( / ) x y
    let ( ** ) x y = float_binop ( ** ) x y
    let min x y = float_binop min x y
    let max x y = float_binop max x y
    let ( > ) x y = cmp_binop ( > ) x y
    let ( >= ) x y = cmp_binop ( >= ) x y
    let ( < ) x y = cmp_binop ( < ) x y
    let ( <= ) x y = cmp_binop ( <= ) x y
    let ( = ) x y = cmp_binop ( = ) x y
  end

  let for_ l s h f =
    wrap @@ fun () ->
    let lo = to_int l in
    let step = to_int s in
    let hi = to_int h in
    let rec loop i =
      if Core.Int.(i >= hi) then Unit
      else (
        to_unit (f Int.(int i));
        loop Core.Int.(i + step))
    in
    loop lo

  let ite c t e = if to_bool c then t () else e ()
  let force = function (lazy { value = Unit; _ }) -> () | _ -> failwith "Expected unit"

  let seq x y =
    wrap' @@ fun () ->
    force x;
    to_value' y

  let sseq = List.fold_left ~init:unit ~f:seq
  let exit = wrap @@ fun () -> raise Exit
  let return = wrap @@ fun () -> assert false

  let print s =
    wrap @@ fun () ->
    print_endline s;
    Unit

  let eprint = print
  let to_func x = match to_value x with Func x -> x | _ -> assert false

  module Func = struct
    let mk_type _ _ = ()
    let func _ _ f = wrap @@ fun () -> Func (fun x -> f (to_code x) |> to_value')
    let apply f x = wrap' @@ fun () -> (to_func f) (to_value' x)
  end

  let add_annot x k v =
    lazy
      (let { value; annot } = Lazy.force x in
       { value; annot = Univ_map.set annot k v })

  let find_annot (lazy { annot; _ }) k = Univ_map.find annot k
end
