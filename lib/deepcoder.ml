open! Core

module Make (C : Sigs.CODE) = struct
  module Value = struct
    type array_t = int32 array array

    type int_t = int32 array

    type t =
      | A of array_t C.t
      | I of int_t C.t
      | F_int of (int32 C.t -> int32 C.t)
      | F_bool of (int32 C.t -> bool C.t)
      | F_int2 of (int32 C.t -> int32 C.t -> int32 C.t)

    type type_ = C.ctype

    let ( = ) v v' =
      match (v, v') with
      | A a, A a' -> Some C.Array.O.(a = a')
      | I x, I x' -> Some C.Array.O.(x = x')
      | F_int _, F_int _ | F_bool _, F_bool _ | F_int2 _, F_int2 _ ->
          failwith "Cannot compare"
      | _ -> None

    let type_of = function
      | A x -> C.type_of x
      | I x -> C.type_of x
      | _ -> assert false

    let code = function A x -> C.cast x | I x -> C.cast x | _ -> assert false

    let eq (type a) (x : t) (y : a C.t) =
      match x with
      | A x -> C.(Array.O.(x = cast y))
      | I x -> C.(Array.O.(x = cast y))
      | _ -> assert false

    let int_type = C.Array.mk_type C.Int

    let array_type = C.Array.mk_type (C.Array.mk_type C.Int)
  end

  module Lang = struct
    include Value

    type value = Value.t

    type 'a code = 'a C.t

    let grammar : Grammar.t =
      let open Grammar in
      let open Grammar.Term in
      [
        ("L", Id "i");
        ("I", App ("head", [ Id "L" ]));
        ("I", App ("last", [ Id "L" ]));
        ("L", App ("take", [ Id "I"; Id "L" ]));
        ("L", App ("drop", [ Id "I"; Id "L" ]));
        ("I", App ("access", [ Id "I"; Id "L" ]));
        ("I", App ("minimum", [ Id "L" ]));
        ("I", App ("maximum", [ Id "L" ]));
        ("L", App ("reverse", [ Id "L" ]));
        (* ("L", App ("sort", [ Id "L" ])); *)
        ("I", App ("sum", [ Id "L" ]));
        ("L", App ("map", [ Id "FII"; Id "L" ]));
        (* ("L", App ("filter", [ Id "FIB"; Id "L" ])); *)
        ("I", App ("count", [ Id "FIB"; Id "L" ]));
        ("L", App ("zipwith", [ Id "FIII"; Id "L"; Id "L" ]));
        (* ("L", App ("scanl1", [ Id "FIII"; Id "L" ])); *)
        ("FII", Id "(+1)");
        ("FII", Id "(-1)");
        ("FII", Id "(*2)");
        ("FII", Id "(/2)");
        ("FII", Id "(*(-1))");
        ("FII", Id "(**2)");
        ("FII", Id "(*3)");
        ("FII", Id "(/3)");
        ("FII", Id "(*4)");
        ("FII", Id "(/4)");
        ("FIB", Id "(>0)");
        ("FIB", Id "(<0)");
        ("FIB", Id "(%2==0)");
        ("FIB", Id "(%2==1)");
        ("FIII", Id "(+)");
        ("FIII", Id "(-)");
        ("FIII", Id "(*)");
        ("FIII", Id "min");
        ("FIII", Id "max");
      ]
      |> inline "FII" |> inline "FIB" |> inline "FIII"
      |> List.filter ~f:(fun (lhs, _) -> String.(lhs = "L" || lhs = "I"))

    open Value

    let to_array = function A x -> x | _ -> assert false

    let to_int = function I x -> x | _ -> assert false

    let to_int_f = function F_int x -> x | _ -> assert false

    let to_bool_f = function F_bool x -> x | _ -> assert false

    let to_int2_f = function F_int2 x -> x | _ -> assert false

    open C
    open Array

    let int_array = Array.mk_type Int

    let rec eval ctx expr =
      match expr with
      | Grammar.Term.Id "(+1)" -> F_int (fun x -> x + int 1)
      | Id "(-1)" -> F_int (fun x -> x - int 1)
      | Id "(*2)" -> F_int (fun x -> x * int 2)
      | Id "(/2)" -> F_int (fun x -> x / int 2)
      | Id "(*(-1))" -> F_int (fun x -> -x)
      | Id "(**2)" -> F_int (fun x -> let_ x (fun x -> x * x))
      | Id "(*3)" -> F_int (fun x -> x * int 3)
      | Id "(/3)" -> F_int (fun x -> x / int 3)
      | Id "(*4)" -> F_int (fun x -> x * int 4)
      | Id "(/4)" -> F_int (fun x -> x / int 4)
      | Id "(>0)" -> F_bool (fun x -> x > int 0)
      | Id "(<0)" -> F_bool (fun x -> x < int 0)
      | Id "(%2==0)" -> F_bool (fun x -> x mod int 2 = int 0)
      | Id "(%2==1)" -> F_bool (fun x -> x mod int 2 = int 1)
      | Id "(+)" -> F_int2 (fun x y -> x + y)
      | Id "(-)" -> F_int2 (fun x y -> x - y)
      | Id "(*)" -> F_int2 (fun x y -> x * y)
      | Id "min" -> F_int2 (fun x y -> ite (x < y) x y)
      | Id "max" -> F_int2 (fun x y -> ite (x > y) x y)
      | Id x -> Map.find_exn ctx x
      | App ("head", [ e ]) ->
          I
            ( eval ctx e |> to_array
            |> map Value.int_type ~f:(fun a -> get a (int 0)) )
      | App ("last", [ e ]) ->
          let a = eval ctx e |> to_array in
          I
            (let_ a (fun a ->
                 map Value.int_type a ~f:(fun a -> get a (length a - int 1))))
      | App ("take", [ n; e ]) ->
          let a = eval ctx e |> to_array in
          let n = eval ctx n |> to_int in
          A (map2 Value.array_type a n ~f:(fun a n -> sub a (int 0) n))
      | App ("drop", [ n; e ]) ->
          let a = eval ctx e |> to_array in
          let n = eval ctx n |> to_int in
          A
            (let_ a (fun a ->
                 let_ n (fun n ->
                     map2 Value.array_type a n ~f:(fun a n ->
                         sub a n (length a - int 1)))))
      | App ("access", [ n; e ]) ->
          let a = eval ctx e |> to_array in
          let n = eval ctx n |> to_int in
          I (map2 Value.int_type a n ~f:(fun a n -> get a n))
      | App ("minimum", [ e ]) ->
          let a = eval ctx e |> to_array in
          I
            (map Value.int_type a ~f:(fun a ->
                 fold
                   ~init:(int Int32.(max_value |> to_int_exn))
                   ~f:(fun acc x -> ite (x < acc) x acc)
                   a))
      | App ("maximum", [ e ]) ->
          let a = eval ctx e |> to_array in
          I
            (map Value.int_type a ~f:(fun a ->
                 fold
                   ~init:(int Int32.(min_value |> to_int_exn))
                   ~f:(fun acc x -> ite (x > acc) x acc)
                   a))
      | App ("reverse", [ e ]) ->
          let a = eval ctx e |> to_array in
          A
            (map Value.array_type a ~f:(fun a ->
                 let_ (length a) (fun l ->
                     init int_array l (fun i -> get a (l - i - int 1)))))
      | App ("sum", [ e ]) ->
          let a = eval ctx e |> to_array in
          I (map Value.int_type a ~f:(fun a -> fold ~init:(int 0) ~f:( + ) a))
      | App ("map", [ f; e ]) ->
          let a = eval ctx e |> to_array in
          let f = eval ctx f |> to_int_f in
          A
            (map Value.array_type a ~f:(fun a ->
                 let_ a (fun a ->
                     init int_array (length a) (fun i -> f (get a i)))))
      | App ("count", [ f; e ]) ->
          let f = eval ctx f |> to_bool_f in
          let a = eval ctx e |> to_array in
          I
            (map Value.int_type a ~f:(fun a ->
                 fold ~init:(int 0)
                   ~f:(fun acc x -> ite (f x) (acc + int 1) acc)
                   a))
      | App ("zipwith", [ f; e; e' ]) ->
          let f = eval ctx f |> to_int2_f in
          let a = eval ctx e |> to_array in
          let a' = eval ctx e' |> to_array in
          A
            (map2 Value.array_type a a' ~f:(fun a a' ->
                 init int_array
                   (min (length a) (length a'))
                   (fun i -> f (get a i) (get a' i))))
      | e ->
          Error.create "Unexpected expression." e [%sexp_of: Grammar.Term.t]
          |> Error.raise
  end

  module Cache = struct
    type value = Value.t

    type 'a code = 'a C.t

    type t = {
      ints : (Value.int_t * int32 array) C.set array code;
      arrays : (Value.array_t * int32 array) C.set array code;
    }

    let max_size = 100

    open C

    let empty k =
      let sizes_t = Array.mk_type Int in
      let mk_type t = Array.mk_type @@ Set.mk_type (Tuple.mk_type t sizes_t) in
      let mk_empty t =
        Array.init t (int max_size) (fun _ -> Set.empty (Array.elem_type t))
      in
      let i_cache_t = mk_type Value.int_type in
      let a_cache_t = mk_type Value.array_type in
      Log.debug (fun m ->
          m "Array type: %s"
            (Sexp.to_string @@ [%sexp_of: ctype] Value.array_type));
      Log.debug (fun m ->
          m "Array cache type: %s"
            (Sexp.to_string @@ [%sexp_of: ctype] a_cache_t));
      let_ (mk_empty i_cache_t) (fun ti ->
          let_ (mk_empty a_cache_t) (fun ta -> k { ints = ti; arrays = ta }))

    let put ~sym:_ ~size ~sizes { ints = tbl_i; arrays = tbl_a; _ } v =
      let key = int size in
      let add tbl v = Set.add tbl.(key) (Tuple.create v sizes) in
      match v with
      | Value.I v -> add tbl_i v
      | A v -> add tbl_a v
      | _ -> assert false

    let iter ~sym ~size:key ~f { ints = tbl_i; arrays = tbl_a; _ } =
      match sym with
      | "I" ->
          Set.iter tbl_i.(key) (fun v -> f (Value.I (Tuple.fst v), Tuple.snd v))
      | "L" ->
          Set.iter tbl_a.(key) (fun v -> f (Value.A (Tuple.fst v), Tuple.snd v))
      | _ -> assert false

    let print_size _ = failwith "print_size"
  end
end
