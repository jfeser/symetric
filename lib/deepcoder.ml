open! Core

module Make (C : Sigs.CODE) = struct
  module Value = struct
    type t =
      | A of int32 array C.t
      | I of int32 C.t
      | F_int of (int32 C.t -> int32 C.t)
      | F_bool of (int32 C.t -> bool C.t)
      | F_int2 of (int32 C.t -> int32 C.t -> int32 C.t)

    let ( = ) v v' =
      match (v, v') with
      | A a, A a' -> C.Array.O.(a = a')
      | I x, I x' -> C.( = ) x x'
      | F_int _, F_int _ | F_bool _, F_bool _ | F_int2 _, F_int2 _ ->
          failwith "Cannot compare"
      | _ -> C.bool false
  end

  module Lang = struct
    include Value

    type value = Value.t

    type 'a code = 'a C.t

    let grammar : Grammar.t =
      let open Grammar in
      let open Grammar.Term in
      [
        ("I", Id "k");
        ("L", Id "b");
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

    let rec eval ctx =
      let open C in
      let open Array in
      let int_array = Array.mk_type Int in
      function
      | Grammar.Term.Id "k" -> I (int 2)
      | Id "b" -> A (const int_array [| int 3; int 5; int 4; int 7; int 5 |])
      | Id "(+1)" -> F_int (fun x -> x + int 1)
      | Id "(-1)" -> F_int (fun x -> x - int 1)
      | Id "(*2)" -> F_int (fun x -> x * int 2)
      | Id "(/2)" -> F_int (fun x -> x / int 2)
      | Id "(*(-1))" -> F_int (fun x -> -x)
      | Id "(**2)" -> F_int (fun x -> x * x)
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
          let a = eval ctx e |> to_array in
          I (get a (int 0))
      | App ("last", [ e ]) ->
          let a = eval ctx e |> to_array in
          I (get a (length a - int 1))
      | App ("take", [ n; e ]) ->
          let a = eval ctx e |> to_array in
          let n = eval ctx n |> to_int in
          A (sub a (int 0) n)
      | App ("drop", [ n; e ]) ->
          let a = eval ctx e |> to_array in
          let n = eval ctx n |> to_int in
          A (sub a n (length a - int 1))
      | App ("access", [ n; e ]) ->
          let a = eval ctx e |> to_array in
          let n = eval ctx n |> to_int in
          I (get a n)
      | App ("minimum", [ e ]) ->
          let a = eval ctx e |> to_array in
          I
            (fold
               ~init:(int Int32.(max_value |> to_int_exn))
               ~f:(fun acc x -> ite (x < acc) x acc)
               a)
      | App ("maximum", [ e ]) ->
          let a = eval ctx e |> to_array in
          I
            (fold
               ~init:(int Int32.(min_value |> to_int_exn))
               ~f:(fun acc x -> ite (x > acc) x acc)
               a)
      | App ("reverse", [ e ]) ->
          let a = eval ctx e |> to_array in
          A
            (let_ (length a) (fun l ->
                 init int_array l (fun i -> get a (l - i - int 1))))
      | App ("sum", [ e ]) ->
          let a = eval ctx e |> to_array in
          I (fold ~init:(int 0) ~f:( + ) a)
      | App ("map", [ f; e ]) ->
          let a = eval ctx e |> to_array in
          let f = eval ctx f |> to_int_f in
          A (init int_array (length a) (fun i -> f (get a i)))
      | App ("count", [ f; e ]) ->
          let f = eval ctx f |> to_bool_f in
          let a = eval ctx e |> to_array in
          I
            (fold ~init:(int 0) ~f:(fun acc x -> ite (f x) (acc + int 1) acc) a)
      | App ("zipwith", [ f; e; e' ]) ->
          let f = eval ctx f |> to_int2_f in
          let a = eval ctx e |> to_array in
          let a' = eval ctx e' |> to_array in
          A (init int_array (length a) (fun i -> f (get a i) (get a' i)))
      | e ->
          Error.create "Unexpected expression." e [%sexp_of: Grammar.Term.t]
          |> Error.raise
  end

  module Cache = struct
    type value = Value.t

    type 'a code = 'a C.t

    type t = {
      ints : (int32 * int32 array) C.set array code;
      arrays : (int32 array * int32 array) C.set array code;
    }

    let max_size = 100

    open C.Array
    open C.Set

    let empty k =
      let int_array = C.Array.mk_type Int in
      let int_set = C.Set.mk_type (C.Tuple.mk_type Int int_array) in
      let int_set_array = C.Array.mk_type int_set in
      let int_array_set =
        C.Set.mk_type (C.Tuple.mk_type int_array int_array)
      in
      let int_array_set_array = C.Array.mk_type int_array_set in
      let tbl_i =
        init int_set_array (C.int max_size) (fun _ -> empty int_set)
      in
      let tbl_a =
        init int_array_set_array (C.int max_size) (fun _ ->
            empty int_array_set)
      in
      k { ints = tbl_i; arrays = tbl_a }

    let put ~sym:_ ~size ~sizes { ints = tbl_i; arrays = tbl_a; _ } v =
      let key = C.int size in
      match v with
      | Value.I v -> add (get tbl_i key) (C.Tuple.create v sizes)
      | A v -> add (get tbl_a key) (C.Tuple.create v sizes)
      | _ -> assert false

    let iter ~sym ~size:key ~f { ints = tbl_i; arrays = tbl_a; _ } =
      match sym with
      | "I" ->
          iter (get tbl_i key) (fun v ->
              f (Value.I (C.Tuple.fst v), C.Tuple.snd v))
      | "L" ->
          iter (get tbl_a key) (fun v ->
              f (Value.A (C.Tuple.fst v), C.Tuple.snd v))
      | _ -> assert false

    let print_size _ = failwith "print_size"
  end
end
