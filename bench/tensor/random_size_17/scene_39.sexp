((ops
  (Flip Reshape Reshape Flip
   (Id ((6 7 2 4 7 5 0 3 7 8 9 1 3 0 6 2 9 1 4 4 2 4 8 3) (4 3 2))) (Int 2)
   Cons (Int 3) Cons (Int 1) Vec (Int 8) Cons (Int 6) Vec (Int 4) (Int 0)))
 (output (Tensor ((4 2 3 8 1 9 4 4 0 3 2 6 8 7 1 9 5 7 3 0 7 6 4 2) (6 4))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id ((6 7 2 4 7 5 0 3 7 8 9 1 3 0 6 2 9 1 4 4 2 4 8 3) (4 3 2)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons
          ((Apply (Int 3) ())
           (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 8) ())))))))))
       (Apply Cons ((Apply (Int 6) ()) (Apply Vec ((Apply (Int 4) ())))))))
     (Apply (Int 0) ()))))))
