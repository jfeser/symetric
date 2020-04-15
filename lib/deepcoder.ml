open! Core

module Make
    (A : Sigs.ARRAY)
    (C : Sigs.CODE with type 'a t = 'a A.t and type 'a ctype = 'a A.ctype) =
struct
  module Value = struct
    type array_t = int32 A.array C.Array.array

    type int_t = int32 C.Array.array

    type value = Value

    type t =
      | A of array_t C.t
      | I of int_t C.t
      | F_int of (int32 C.t -> int32 C.t)
      | F_bool of (int32 C.t -> bool C.t)
      | F_int2 of (int32 C.t -> int32 C.t -> int32 C.t)

    type mapper = { f : 'a. 'a C.t -> 'a C.t }

    let int_t = C.Array.mk_type C.Int.type_

    let array_t = C.Array.mk_type (A.mk_type C.Int.type_)

    let random ?(state = Random.State.default) sym n =
      let int_t = C.Int.type_ in
      let random_int () =
        let x = Random.State.int_incl state 0 10 in
        C.Int.int x
      in
      let random_array () =
        let length = Random.State.int_incl state 0 8 in
        let arr = Array.init length ~f:(fun _ -> random_int ()) in
        A.const (A.mk_type int_t) arr
      in
      let random_examples elem elem_t () =
        C.Array.const (C.Array.mk_type elem_t)
          (Array.init n ~f:(fun _ -> elem ()))
      in
      if String.(sym = "L") then
        A (random_examples random_array (A.mk_type int_t) ())
      else I (random_examples random_int int_t ())

    let key =
      Univ_map.Key.create ~name:"deepcoder.value" [%sexp_of: [ `A | `I ]]

    let code_of = function
      | A x -> C.add_annot (C.cast x) key `A
      | I x -> C.add_annot (C.cast x) key `I
      | _ -> failwith "Not convertible"

    let of_code c =
      match C.find_annot c key with
      | Some `A -> A (C.cast c)
      | Some `I -> I (C.cast c)
      | None -> failwith "Not convertible."

    let map ~f v =
      match v with
      | A x -> A (f.f x)
      | I x -> I (f.f x)
      | _ -> failwith "Not a code value."

    let ( = ) v v' =
      match (v, v') with
      | A a, A a' -> `Dyn C.Array.O.(a = a')
      | I x, I x' -> `Dyn C.Array.O.(x = x')
      | A _, I _ | I _, A _ -> `Static false
      | _ -> failwith "Cannot compare"

    let code = function A x -> C.cast x | I x -> C.cast x | _ -> assert false

    let eq (type a) (x : t) (y : a C.t) =
      match x with
      | A x -> C.Array.O.(x = C.cast y)
      | I x -> C.Array.O.(x = C.cast y)
      | _ -> assert false

    let let_ v f =
      match v with
      | A x -> C.let_ x (fun v' -> f (A v'))
      | I x -> C.let_ x (fun v' -> f (I v'))
      | _ -> assert false

    let of_sexp symbol s =
      let array_of_sexp examples =
        let int_t = C.Int.type_ in
        C.Array.of_sexp array_t examples (fun array ->
            A.of_sexp (A.mk_type int_t) array C.Int.of_sexp)
      in
      let int_of_sexp examples = C.Array.of_sexp int_t examples C.Int.of_sexp in

      if String.(symbol = "L") then A (array_of_sexp s) else I (int_of_sexp s)

    let sexp_of v =
      let array_to_sexp examples =
        C.Array.sexp_of examples (fun array -> A.sexp_of array C.Int.sexp_of)
      in
      let int_to_sexp examples = C.Array.sexp_of examples C.Int.sexp_of in

      match v with
      | A x -> array_to_sexp x
      | I x -> int_to_sexp x
      | _ -> failwith "unexpected value"
  end

  module Lang = struct
    module Value = Value

    type value = Value.t

    type 'a code = 'a C.t

    let grammar : Grammar.t =
      let open Grammar in
      let open Grammar.Term in
      let nt x = Nonterm x in
      let id x = App (x, []) in
      [
        ("I", App ("head", [ nt "L" ]));
        ("I", App ("last", [ nt "L" ]));
        ("L", App ("take", [ nt "I"; nt "L" ]));
        ("L", App ("drop", [ nt "I"; nt "L" ]));
        ("I", App ("access", [ nt "I"; nt "L" ]));
        ("I", App ("minimum", [ nt "L" ]));
        ("I", App ("maximum", [ nt "L" ]));
        ("L", App ("reverse", [ nt "L" ]));
        (* ("L", App ("sort", [ nt "L" ])); *)
        ("I", App ("sum", [ nt "L" ]));
        ("L", App ("map", [ nt "FII"; nt "L" ]));
        (* ("L", App ("filter", [ nt "FIB"; nt "L" ])); *)
        ("I", App ("count", [ nt "FIB"; nt "L" ]));
        ("L", App ("zipwith", [ nt "FIII"; nt "L"; nt "L" ]));
        (* ("L", App ("scanl1", [ nt "FIII"; nt "L" ])); *)
        ("FII", id "(+1)");
        ("FII", id "(-1)");
        ("FII", id "(*2)");
        ("FII", id "(/2)");
        ("FII", id "(*(-1))");
        ("FII", id "(**2)");
        ("FII", id "(*3)");
        ("FII", id "(/3)");
        ("FII", id "(*4)");
        ("FII", id "(/4)");
        ("FIB", id "(>0)");
        ("FIB", id "(<0)");
        ("FIB", id "(%2==0)");
        ("FIB", id "(%2==1)");
        ("FIII", id "(+)");
        ("FIII", id "(-)");
        ("FIII", id "(*)");
        ("FIII", id "min");
        ("FIII", id "max");
      ]
      |> inline "FII" |> inline "FIB" |> inline "FIII"

    open Value

    let to_array = function A x -> x | _ -> assert false

    let to_int = function I x -> x | _ -> assert false

    let to_int_f = function F_int x -> x | _ -> assert false

    let to_bool_f = function F_bool x -> x | _ -> assert false

    let to_int2_f = function F_int2 x -> x | _ -> assert false

    open C
    open A

    let int_array = A.mk_type Int.type_

    let rec eval ctx =
      let open Int in
      function
      | Grammar.Term.App ("(+1)", []) -> F_int (fun x -> x + int 1)
      | App ("(-1)", []) -> F_int (fun x -> x - int 1)
      | App ("(*2)", []) -> F_int (fun x -> x * int 2)
      | App ("(/2)", []) -> F_int (fun x -> x / int 2)
      | App ("(*(-1))", []) -> F_int (fun x -> -x)
      | App ("(**2)", []) -> F_int (fun x -> let_ x (fun x -> x * x))
      | App ("(*3)", []) -> F_int (fun x -> x * int 3)
      | App ("(/3)", []) -> F_int (fun x -> x / int 3)
      | App ("(*4)", []) -> F_int (fun x -> x * int 4)
      | App ("(/4)", []) -> F_int (fun x -> x / int 4)
      | App ("(>0)", []) -> F_bool (fun x -> x > int 0)
      | App ("(<0)", []) -> F_bool (fun x -> x < int 0)
      | App ("(%2==0)", []) -> F_bool (fun x -> x mod int 2 = int 0)
      | App ("(%2==1)", []) -> F_bool (fun x -> x mod int 2 = int 1)
      | App ("(+)", []) -> F_int2 (fun x y -> x + y)
      | App ("(-)", []) -> F_int2 (fun x y -> x - y)
      | App ("(*)", []) -> F_int2 (fun x y -> x * y)
      | App ("min", []) ->
          F_int2 (fun x y -> ite (x < y) (fun () -> x) (fun () -> y))
      | App ("max", []) ->
          F_int2 (fun x y -> ite (x > y) (fun () -> x) (fun () -> y))
      | App (x, []) -> (
          match Map.find ctx x with
          | Some v -> v
          | None ->
              Error.create "Unbound name." (x, ctx)
                [%sexp_of: string * (value[@opaque]) Map.M(Core.String).t]
              |> Error.raise )
      | App ("head", [ e ]) ->
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              C.Array.map Value.int_t a ~f:(fun a ->
                  ite
                    (length a > int 0)
                    (fun () -> get a (int 0))
                    (fun () -> int 0)) )
      | App ("last", [ e ]) ->
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              C.Array.map Value.int_t a ~f:(fun a ->
                  let_ (length a) (fun l ->
                      ite
                        (l > int 0)
                        (fun () -> get a (length a - int 1))
                        (fun () -> int 0))) )
      | App ("take", [ n; e ]) ->
          A
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              let_ (eval ctx n |> to_int) @@ fun n ->
              C.Array.map2 Value.array_t a n ~f:(fun a n -> sub a (int 0) n) )
      | App ("drop", [ n; e ]) ->
          A
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              let_ (eval ctx n |> to_int) @@ fun n ->
              C.Array.map2 Value.array_t a n ~f:(fun a n ->
                  sub a n (length a - int 1)) )
      | App ("access", [ n; e ]) ->
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              let_ (eval ctx n |> to_int) @@ fun n ->
              C.Array.map2 Value.int_t a n ~f:(fun a n ->
                  ite
                    Bool.(n >= int 0 && n < length a)
                    (fun () -> get a n)
                    (fun () -> int 0)) )
      | App ("minimum", [ e ]) ->
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              C.Array.map Value.int_t a ~f:(fun a ->
                  fold
                    ~init:(int Int32.(max_value |> to_int_exn))
                    ~f:(fun acc x ->
                      ite (x < acc) (fun () -> x) (fun () -> acc))
                    a) )
      | App ("maximum", [ e ]) ->
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              C.Array.map Value.int_t a ~f:(fun a ->
                  fold
                    ~init:(int Int32.(min_value |> to_int_exn))
                    ~f:(fun acc x ->
                      ite (x > acc) (fun () -> x) (fun () -> acc))
                    a) )
      | App ("reverse", [ e ]) ->
          A
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              C.Array.map Value.array_t a ~f:(fun a ->
                  let_ (length a) (fun l ->
                      init int_array l (fun i -> get a (l - i - int 1)))) )
      | App ("sum", [ e ]) ->
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              C.Array.map Value.int_t a ~f:(fun a ->
                  fold ~init:(int 0) ~f:( + ) a) )
      | App ("map", [ f; e ]) ->
          let f = eval ctx f |> to_int_f in
          A
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              C.Array.map Value.array_t a ~f:(fun a ->
                  let_ a (fun a ->
                      init int_array (length a) (fun i -> f (get a i)))) )
      | App ("count", [ f; e ]) ->
          let f = eval ctx f |> to_bool_f in
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              C.Array.map Value.int_t a ~f:(fun a ->
                  fold ~init:(int 0)
                    ~f:(fun acc x ->
                      ite (f x) (fun () -> acc + int 1) (fun () -> acc))
                    a) )
      | App ("zipwith", [ f; e; e' ]) ->
          let f = eval ctx f |> to_int2_f in
          A
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              let_ (eval ctx e' |> to_array) @@ fun a' ->
              C.Array.map2 Value.array_t a a' ~f:(fun a a' ->
                  init int_array
                    (min (length a) (length a'))
                    (fun i -> f (get a i) (get a' i))) )
      | e ->
          Error.create "Unexpected expression." e [%sexp_of: Grammar.Term.t]
          |> Error.raise

    let eval ctx expr =
      try eval ctx expr
      with exn ->
        let open Error in
        let err = of_exn exn in
        tag_arg err "Evaluation failed" expr [%sexp_of: Grammar.Term.t] |> raise
  end

  module Cache = struct
    type value = Value.t

    type 'a code = 'a C.t

    type cache =
      Value.int_t Sigs.set C.Array.array * Value.array_t Sigs.set C.Array.array

    type t = {
      ints : Value.int_t Sigs.set C.Array.array code;
      arrays : Value.array_t Sigs.set C.Array.array code;
    }

    let max_size = 100

    open C

    let empty () =
      let mk_type t = Array.mk_type @@ Set.mk_type t in
      let mk_empty t =
        Array.init t (Int.int max_size) (fun _ -> Set.empty (Array.elem_type t))
      in
      let i_cache_t = mk_type Value.int_t in
      let a_cache_t = mk_type Value.array_t in
      let i_cache = Nonlocal_let.let_ let_ (fun () -> mk_empty i_cache_t) in
      let a_cache = Nonlocal_let.let_ let_ (fun () -> mk_empty a_cache_t) in
      Nonlocal_let.
        {
          value =
            (fun () -> { ints = i_cache.value (); arrays = a_cache.value () });
          bind = (fun f -> i_cache.bind (fun () -> a_cache.bind f));
        }

    let put ~sym:_ ~size { ints = tbl_i; arrays = tbl_a; _ } v =
      let key = Int.int size in
      let add tbl = Set.add tbl.(key) in
      match v with
      | Value.I v -> add tbl_i v
      | A v -> add tbl_a v
      | _ -> assert false

    let iter ~sym ~size:key ~f { ints = tbl_i; arrays = tbl_a; _ } =
      match sym with
      | "I" -> Set.iter tbl_i.(key) (fun v -> f (Value.I v))
      | "L" -> Set.iter tbl_a.(key) (fun v -> f (Value.A v))
      | _ -> assert false

    let print_size _ = failwith "print_size"

    let code_of { ints; arrays } = C.Tuple.create ints arrays

    let of_code t = { ints = C.Tuple.fst t; arrays = C.Tuple.snd t }
  end
end
