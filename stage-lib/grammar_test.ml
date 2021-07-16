open! Core
open Grammar

let%expect_test "" =
  let open Term in
  inline "A" @@ Grammar.of_list [ ("A", app "x" []); ("B", app "f" [ nonterm "A" ]) ]
  |> [%sexp_of: _ Grammar.t] |> print_s;
  [%expect {| (((lhs A) (rhs (App x ()))) ((lhs B) (rhs (App f ((App x ())))))) |}]

let%expect_test "" =
  let open Term in
  inline "A" @@ Grammar.of_list [ ("A", app "x" []); ("A", app "y" []); ("B", app "f" [ nonterm "A" ]) ]
  |> [%sexp_of: _ Grammar.t] |> print_s;
  [%expect
    {|
    (((lhs A) (rhs (App x ()))) ((lhs A) (rhs (App y ())))
     ((lhs B) (rhs (App f ((App x ()))))) ((lhs B) (rhs (App f ((App y ())))))) |}]

let test_grammar () =
  let nt x = Term.nonterm x in
  let id x = Term.app x [] in
  let app (x, y) = Term.app x y in
  Grammar.of_list
    [
      ("I", app ("input1", []));
      ("L", app ("input2", []));
      ("I", app ("head", [ nt "L" ]));
      ("I", app ("last", [ nt "L" ]));
      ("L", app ("take", [ nt "I"; nt "L" ]));
      ("L", app ("drop", [ nt "I"; nt "L" ]));
      ("I", app ("access", [ nt "I"; nt "L" ]));
      ("I", app ("minimum", [ nt "L" ]));
      ("I", app ("maximum", [ nt "L" ]));
      ("L", app ("reverse", [ nt "L" ]));
      ("I", app ("sum", [ nt "L" ]));
      ("L", app ("map", [ nt "FII"; nt "L" ]));
      ("I", app ("count", [ nt "FIB"; nt "L" ]));
      ("L", app ("zipwith", [ nt "FIII"; nt "L"; nt "L" ]));
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

let%expect_test "" =
  inline "FII" @@ test_grammar () |> [%sexp_of: _ t] |> print_s;
  [%expect
    {|
    (((lhs I) (rhs (App input1 ()))) ((lhs L) (rhs (App input2 ())))
     ((lhs I) (rhs (App head ((Nonterm L)))))
     ((lhs I) (rhs (App last ((Nonterm L)))))
     ((lhs L) (rhs (App take ((Nonterm I) (Nonterm L)))))
     ((lhs L) (rhs (App drop ((Nonterm I) (Nonterm L)))))
     ((lhs I) (rhs (App access ((Nonterm I) (Nonterm L)))))
     ((lhs I) (rhs (App minimum ((Nonterm L)))))
     ((lhs I) (rhs (App maximum ((Nonterm L)))))
     ((lhs L) (rhs (App reverse ((Nonterm L)))))
     ((lhs I) (rhs (App sum ((Nonterm L)))))
     ((lhs L) (rhs (App map ((App "(+1)" ()) (Nonterm L)))))
     ((lhs L) (rhs (App map ((App "(-1)" ()) (Nonterm L)))))
     ((lhs L) (rhs (App map ((App "(*2)" ()) (Nonterm L)))))
     ((lhs L) (rhs (App map ((App "(/2)" ()) (Nonterm L)))))
     ((lhs L) (rhs (App map ((App "(*(-1))" ()) (Nonterm L)))))
     ((lhs L) (rhs (App map ((App "(**2)" ()) (Nonterm L)))))
     ((lhs L) (rhs (App map ((App "(*3)" ()) (Nonterm L)))))
     ((lhs L) (rhs (App map ((App "(/3)" ()) (Nonterm L)))))
     ((lhs L) (rhs (App map ((App "(*4)" ()) (Nonterm L)))))
     ((lhs L) (rhs (App map ((App "(/4)" ()) (Nonterm L)))))
     ((lhs I) (rhs (App count ((Nonterm FIB) (Nonterm L)))))
     ((lhs L) (rhs (App zipwith ((Nonterm FIII) (Nonterm L) (Nonterm L)))))
     ((lhs FII) (rhs (App "(+1)" ()))) ((lhs FII) (rhs (App "(-1)" ())))
     ((lhs FII) (rhs (App "(*2)" ()))) ((lhs FII) (rhs (App "(/2)" ())))
     ((lhs FII) (rhs (App "(*(-1))" ()))) ((lhs FII) (rhs (App "(**2)" ())))
     ((lhs FII) (rhs (App "(*3)" ()))) ((lhs FII) (rhs (App "(/3)" ())))
     ((lhs FII) (rhs (App "(*4)" ()))) ((lhs FII) (rhs (App "(/4)" ())))
     ((lhs FIB) (rhs (App "(>0)" ()))) ((lhs FIB) (rhs (App "(<0)" ())))
     ((lhs FIB) (rhs (App "(%2==0)" ()))) ((lhs FIB) (rhs (App "(%2==1)" ())))
     ((lhs FIII) (rhs (App "(+)" ()))) ((lhs FIII) (rhs (App "(-)" ())))
     ((lhs FIII) (rhs (App "(*)" ()))) ((lhs FIII) (rhs (App min ())))
     ((lhs FIII) (rhs (App max ())))) |}]

let%expect_test "" =
  weighted_random ~state:(Random.State.make [||]) [ (0.2, "a"); (0.3, "b"); (0.5, "c") ] |> print_endline;
  [%expect {| a |}]

let%expect_test "" =
  let open Untyped_term in
  map (App ("x", [ Nonterm "y"; Nonterm "z" ])) ~nonterm:(fun x -> Nonterm (x ^ "!")) |> to_string |> print_endline;
  [%expect {| x(y!, z!) |}]

let%expect_test "" =
  let state = Random.State.make [||] in
  let i = ref 0 in
  while !i < 10 do
    let t = sample ~state ~factor:0.001 "L" (test_grammar ()) in
    let n = Term.size t in
    if n >= 5 && n <= 8 then (
      incr i;
      Term.to_string t |> print_endline;
      [%expect
        {|
        (* CR expect_test: Collector ran multiple times with different outputs *)
        =========================================================================
        drop(access(input1, reverse(input2)), reverse(input2))

        =========================================================================
        zipwith(max, map((/3), input2), input2)

        =========================================================================
        take(access(last(input2), input2), input2)

        =========================================================================
        reverse(drop(head(input2), input2))

        =========================================================================
        take(input1, reverse(drop(maximum(input2), input2)))

        =========================================================================
        zipwith((+), take(input1, input2), input2)

        =========================================================================
        reverse(zipwith(min, input2, input2))

        =========================================================================
        take(count((%2==0), zipwith(max, input2, input2)), input2)

        =========================================================================
        reverse(take(minimum(input2), map((-1), input2)))

        =========================================================================
        take(input1, reverse(map((*2), input2))) |}])
  done
