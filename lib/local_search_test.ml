open Local_synth_tensor.Tensor

open Local_search_diverse.Make (Local_synth_tensor.Tensor)

let example =
  {|
  ((Apply (Cons ((Apply ((Int 1) ())) (Apply (Vec ((Var 0)))))))
   ((search_state
     ((max_cost 12)
      (values
       ((((cost 1) (type_ Tensor))
         ((Tensor ((elems (0 1 2 3 4 5 6 7 8 9)) (shape (10))))))
        (((cost 1) (type_ Int))
         ((Int 1) (Int 2) (Int 3) (Int 4) (Int 5) (Int 6) (Int 7) (Int 8)
          (Int 9) (Int 10)))
        (((cost 2) (type_ Vector))
         ((Vector (1)) (Vector (2)) (Vector (3)) (Vector (4)) (Vector (5))
          (Vector (6)) (Vector (7)) (Vector (8)) (Vector (9)) (Vector (10))))))
      (paths
       ((((type_ Tensor)
          (value (Tensor ((elems (0 1 2 3 4 5 6 7 8 9)) (shape (10))))))
         ((1 (Id ((elems (0 1 2 3 4 5 6 7 8 9)) (shape (10)))) ())))
        (((type_ Vector) (value (Vector (2)))) ((2 Vec ((Int 2)))))
        (((type_ Vector) (value (Vector (3)))) ((2 Vec ((Int 3)))))
        (((type_ Vector) (value (Vector (4)))) ((2 Vec ((Int 4)))))
        (((type_ Vector) (value (Vector (8)))) ((2 Vec ((Int 8)))))
        (((type_ Vector) (value (Vector (7)))) ((2 Vec ((Int 7)))))
        (((type_ Vector) (value (Vector (6)))) ((2 Vec ((Int 6)))))
        (((type_ Vector) (value (Vector (5)))) ((2 Vec ((Int 5)))))
        (((type_ Vector) (value (Vector (9)))) ((2 Vec ((Int 9)))))
        (((type_ Vector) (value (Vector (10)))) ((2 Vec ((Int 10)))))
        (((type_ Vector) (value (Vector (1)))) ((2 Vec ((Int 1)))))
        (((type_ Int) (value (Int 9))) ((1 (Int 9) ())))
        (((type_ Int) (value (Int 7))) ((1 (Int 7) ())))
        (((type_ Int) (value (Int 6))) ((1 (Int 6) ())))
        (((type_ Int) (value (Int 5))) ((1 (Int 5) ())))
        (((type_ Int) (value (Int 2))) ((1 (Int 2) ())))
        (((type_ Int) (value (Int 4))) ((1 (Int 4) ())))
        (((type_ Int) (value (Int 3))) ((1 (Int 3) ())))
        (((type_ Int) (value (Int 1))) ((1 (Int 1) ())))
        (((type_ Int) (value (Int 8))) ((1 (Int 8) ())))
        (((type_ Int) (value (Int 10))) ((1 (Int 10) ())))))))
    (value (List (((Vector (1 2)) Cons ((Int 1) (Vector (2)))))))))
|}

let%expect_test "" =
  let lhs, term_set = [%of_sexp: Op.t Local_search.Pattern.t * Term_set.t] @@ Sexp.of_string example in
  Local_search.Pattern.match_ (module Op) (module Term_set) (module Subst) lhs term_set
  |> Iter.iter (fun s -> print_s [%message (s : Subst.t)])
