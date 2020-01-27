open! Core
open Synth

let%expect_test "" =
  to_contexts (App ("x", [])) []
  |> [%sexp_of: Grammar.Term.t * int Map.M(String).t list] |> print_s;
  [%expect {| ((App x ()) (())) |}]

let%expect_test "" =
  to_contexts
    (App ("access", [ Nonterm "I"; Nonterm "L" ]))
    [ ({ cost = 1; symbol = "I" }, 0); ({ cost = 1; symbol = "L" }, 1) ]
  |> [%sexp_of: Grammar.Term.t * int Map.M(String).t list] |> print_s;
  [%expect {| ((App access ((App I0 ()) (App L1 ()))) (((I0 0) (L1 1)))) |}]

let%expect_test "" =
  to_contexts
    (App ("sum", [ Nonterm "I"; Nonterm "I" ]))
    [ ({ cost = 1; symbol = "I" }, 0); ({ cost = 1; symbol = "I" }, 1) ]
  |> [%sexp_of: Grammar.Term.t * int Map.M(String).t list] |> print_s;
  [%expect
    {| ((App sum ((App I0 ()) (App I1 ()))) (((I0 0) (I1 1)) ((I0 1) (I1 0)))) |}]

let%expect_test "" =
  to_contexts
    (App ("sum3", [ Nonterm "I"; Nonterm "I"; Nonterm "J" ]))
    [
      ({ cost = 1; symbol = "I" }, 0);
      ({ cost = 1; symbol = "I" }, 1);
      ({ cost = 1; symbol = "J" }, 2);
    ]
  |> [%sexp_of: Grammar.Term.t * int Map.M(String).t list] |> print_s;
  [%expect
    {|
    ((App sum3 ((App I0 ()) (App I1 ()) (App J2 ())))
     (((I0 0) (I1 1) (J2 2)) ((I0 1) (I1 0) (J2 2)))) |}]
