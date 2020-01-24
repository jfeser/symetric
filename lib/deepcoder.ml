open! Core

module Make (C : Sigs.CODE) = struct
  module Value = struct
    type array_t = int32 array array [@@deriving sexp]

    type int_t = int32 array [@@deriving sexp]

    type t =
      | A of array_t C.t
      | I of int_t C.t
      | F_int of (int32 C.t -> int32 C.t)
      | F_bool of (int32 C.t -> bool C.t)
      | F_int2 of (int32 C.t -> int32 C.t -> int32 C.t)
    [@@deriving sexp_of]

    type type_ = C.ctype

    type mapper = { f : 'a. 'a C.t -> 'a C.t }

    let random ?(state = Random.State.default) sym n =
      let random_int () =
        let x = Random.State.int_incl state 0 10 in
        C.(Int.int x)
      in
      let random_array () =
        let length = Random.State.int_incl state 0 8 in
        let arr = Array.init length ~f:(fun _ -> random_int ()) in
        C.(Array.const (Array.mk_type int_t) arr)
      in
      let random_examples elem elem_t () =
        C.Array.const (C.Array.mk_type elem_t)
          (Array.init n ~f:(fun _ -> elem ()))
      in
      if String.(sym = "L") then
        A (random_examples random_array C.(Array.mk_type int_t) ())
      else I (random_examples random_int C.(int_t) ())

    let map ~f v =
      match v with
      | A x -> A (f.f x)
      | I x -> I (f.f x)
      | _ -> failwith "Not a code value."

    let ( = ) v v' =
      match (v, v') with
      | A a, A a' -> Some C.Array.O.(a = a')
      | I x, I x' -> Some C.Array.O.(x = x')
      | A _, I _ | I _, A _ -> None
      | _ -> failwith "Cannot compare"

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

    let let_ v f =
      match v with
      | A x -> C.let_ x (fun v' -> f (A v'))
      | I x -> C.let_ x (fun v' -> f (I v'))
      | _ -> assert false

    let int_type = C.Array.mk_type C.int_t

    let array_type = C.Array.mk_type (C.Array.mk_type C.int_t)

    let of_sexp symbol s =
      let array_of_sexp examples =
        let open C in
        let open Array in
        of_sexp
          (mk_type (mk_type int_t))
          examples
          (fun array -> of_sexp (mk_type int_t) array Int.of_sexp)
      in
      let int_of_sexp examples =
        let open C in
        let open Array in
        of_sexp (mk_type int_t) examples Int.of_sexp
      in

      if String.(symbol = "L") then A (array_of_sexp s) else I (int_of_sexp s)

    let sexp_of v =
      let array_to_sexp examples =
        let open C in
        let open Array in
        sexp_of examples (fun array -> sexp_of array Int.sexp_of)
      in
      let int_to_sexp examples =
        let open C in
        let open Array in
        sexp_of examples Int.sexp_of
      in

      match v with
      | A x -> array_to_sexp x
      | I x -> int_to_sexp x
      | _ -> failwith "unexpected value"
  end

  module Lang = struct
    module Value = Value

    type value = Value.t [@@deriving sexp_of]

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
    open Array

    let int_array = Array.mk_type int_t

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
                [%sexp_of: string * value Map.M(Core.String).t]
              |> Error.raise )
      | App ("head", [ e ]) ->
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              map Value.int_type a ~f:(fun a ->
                  ite
                    (length a > int 0)
                    (fun () -> get a (int 0))
                    (fun () -> int 0)) )
      | App ("last", [ e ]) ->
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              map Value.int_type a ~f:(fun a ->
                  let_ (length a) (fun l ->
                      ite
                        (l > int 0)
                        (fun () -> get a (length a - int 1))
                        (fun () -> int 0))) )
      | App ("take", [ n; e ]) ->
          A
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              let_ (eval ctx n |> to_int) @@ fun n ->
              map2 Value.array_type a n ~f:(fun a n -> sub a (int 0) n) )
      | App ("drop", [ n; e ]) ->
          A
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              let_ (eval ctx n |> to_int) @@ fun n ->
              map2 Value.array_type a n ~f:(fun a n ->
                  sub a n (length a - int 1)) )
      | App ("access", [ n; e ]) ->
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              let_ (eval ctx n |> to_int) @@ fun n ->
              map2 Value.int_type a n ~f:(fun a n ->
                  ite
                    Bool.(n >= int 0 && n < length a)
                    (fun () -> get a n)
                    (fun () -> int 0)) )
      | App ("minimum", [ e ]) ->
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              map Value.int_type a ~f:(fun a ->
                  fold
                    ~init:(int Int32.(max_value |> to_int_exn))
                    ~f:(fun acc x ->
                      ite (x < acc) (fun () -> x) (fun () -> acc))
                    a) )
      | App ("maximum", [ e ]) ->
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              map Value.int_type a ~f:(fun a ->
                  fold
                    ~init:(int Int32.(min_value |> to_int_exn))
                    ~f:(fun acc x ->
                      ite (x > acc) (fun () -> x) (fun () -> acc))
                    a) )
      | App ("reverse", [ e ]) ->
          A
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              map Value.array_type a ~f:(fun a ->
                  let_ (length a) (fun l ->
                      init int_array l (fun i -> get a (l - i - int 1)))) )
      | App ("sum", [ e ]) ->
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              map Value.int_type a ~f:(fun a -> fold ~init:(int 0) ~f:( + ) a)
            )
      | App ("map", [ f; e ]) ->
          let f = eval ctx f |> to_int_f in
          A
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              map Value.array_type a ~f:(fun a ->
                  let_ a (fun a ->
                      init int_array (length a) (fun i -> f (get a i)))) )
      | App ("count", [ f; e ]) ->
          let f = eval ctx f |> to_bool_f in
          I
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              map Value.int_type a ~f:(fun a ->
                  fold ~init:(int 0)
                    ~f:(fun acc x ->
                      ite (f x) (fun () -> acc + int 1) (fun () -> acc))
                    a) )
      | App ("zipwith", [ f; e; e' ]) ->
          let f = eval ctx f |> to_int2_f in
          A
            ( let_ (eval ctx e |> to_array) @@ fun a ->
              let_ (eval ctx e' |> to_array) @@ fun a' ->
              map2 Value.array_type a a' ~f:(fun a a' ->
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

    type t = {
      ints : (Value.int_t * int32 array) Sigs.set array code;
      arrays : (Value.array_t * int32 array) Sigs.set array code;
    }

    let max_size = 100

    open C

    let empty k =
      let sizes_t = Array.mk_type int_t in
      let mk_type t = Array.mk_type @@ Set.mk_type (Tuple.mk_type t sizes_t) in
      let mk_empty t =
        Array.init t (Int.int max_size) (fun _ -> Set.empty (Array.elem_type t))
      in
      let i_cache_t = mk_type Value.int_type in
      let a_cache_t = mk_type Value.array_type in
      Log.debug (fun m ->
          m "Array type: %s"
            (Core.Sexp.to_string @@ [%sexp_of: ctype] Value.array_type));
      Log.debug (fun m ->
          m "Array cache type: %s"
            (Core.Sexp.to_string @@ [%sexp_of: ctype] a_cache_t));
      let_global (mk_empty i_cache_t) (fun ti ->
          let_global (mk_empty a_cache_t) (fun ta ->
              k { ints = ti; arrays = ta }))

    let put ~sym:_ ~size ~sizes { ints = tbl_i; arrays = tbl_a; _ } v =
      let key = Int.int size in
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
