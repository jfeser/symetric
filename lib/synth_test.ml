open! Core
open Synth
open Grammar.Term

(* let%expect_test "" =
 *   to_contexts (app "x" []) []
 *   |> [%sexp_of: _ t * int Map.M(String).t list] |> print_s;
 *   [%expect {| ((App x ()) (())) |}]
 * 
 * let%expect_test "" =
 *   to_contexts (app "access" [ nonterm "I"; nonterm "L" ]) [ ("I", 0); ("L", 1) ]
 *   |> [%sexp_of: _ t * int Map.M(String).t list] |> print_s;
 *   [%expect {| ((App access ((App I0 ()) (App L1 ()))) (((I0 0) (L1 1)))) |}]
 * 
 * let%expect_test "" =
 *   to_contexts (app "sum" [ nonterm "I"; nonterm "I" ]) [ ("I", 0); ("I", 1) ]
 *   |> [%sexp_of: _ t * int Map.M(String).t list] |> print_s;
 *   [%expect
 *     {| ((App sum ((App I0 ()) (App I1 ()))) (((I0 0) (I1 1)) ((I0 1) (I1 0)))) |}]
 * 
 * let%expect_test "" =
 *   to_contexts
 *     (app "sum3" [ nonterm "I"; nonterm "I"; nonterm "J" ])
 *     [ ("I", 0); ("I", 1); ("J", 2) ]
 *   |> [%sexp_of: _ t * int Map.M(String).t list] |> print_s;
 *   [%expect
 *     {|
 *     ((App sum3 ((App I0 ()) (App I1 ()) (App J2 ())))
 *      (((I0 0) (I1 1) (J2 2)) ((I0 1) (I1 0) (J2 2)))) |}] *)
