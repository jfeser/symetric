open! Core

module Make (C : Sigs.CODE) = struct
  module Value = struct
    type t =
      | A of int array C.t
      | I of int C.t
      | F_int of (int C.t -> int C.t)
      | F_bool of (int C.t -> bool C.t)
      | F_int2 of (int C.t -> int C.t -> int C.t)
  end

  module Lang = struct
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
            (fold ~init:(int Int.max_value)
               ~f:(fun acc x -> ite (x < acc) x acc)
               a)
      | App ("maximum", [ e ]) ->
          let a = eval ctx e |> to_array in
          I
            (fold ~init:(int Int.min_value)
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
      target : Value.t;
      ints : int C.set array code;
      arrays : int array C.set array code;
    }

    let max_size = 100

    open C
    open C.Array
    open C.Set

    let empty target k =
      let int_set = Set.mk_type Int in
      let int_set_array = Array.mk_type int_set in
      let int_array = Array.mk_type Int in
      let int_array_set = Set.mk_type int_array in
      let int_array_set_array = Array.mk_type int_array_set in
      let tbl_i = init int_set_array (int max_size) (fun _ -> empty int_set) in
      let tbl_a =
        init int_array_set_array (int max_size) (fun _ -> empty int_array_set)
      in
      k { target; ints = tbl_i; arrays = tbl_a }

    let put ~sym:_ ~size ~sizes:_ { target; ints = tbl_i; arrays = tbl_a; _ } v
        =
      let key = int size in
      let add_int v = add (get tbl_i key) v in
      let add_array v = add (get tbl_a key) v in
      match (v, target) with
      | Value.I v, Value.I v' -> ite (v = v') unit (add_int v)
      | Value.I v, _ -> add_int v
      | A v, A v' -> ite Array.O.(v = v') unit (add_array v)
      | A v, _ -> add_array v
      | _ -> assert false

    let iter ~sym ~size ~f { ints = tbl_i; arrays = tbl_a; _ } =
      let key = int size in
      match sym with
      | "I" -> iter (get tbl_i key) (fun v -> f (Value.I v))
      | "L" -> iter (get tbl_a key) (fun v -> f (Value.A v))
      | _ -> assert false

    let print_size _ = failwith "print_size"
  end
end
