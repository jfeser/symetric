((ops
  (Flip Reshape Reshape Flip
   (Id ((5 4 8 2 6 0 0 2 2 1 5 4 1 3 5 0 5 1 1 8 4 3 2 0) (2 4 3))) (Int 2)
   Cons (Int 6) Cons (Int 4) Vec (Int 1) Cons (Int 3) Vec (Int 8) (Int 0)))
 (output (Tensor ((5 0 4 8 1 0 2 3 0 4 5 1 5 3 1 1 8 4 5 0 6 2 2 2) (3 8))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id ((5 4 8 2 6 0 0 2 2 1 5 4 1 3 5 0 5 1 1 8 4 3 2 0) (2 4 3)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons
          ((Apply (Int 6) ())
           (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 1) ())))))))))
       (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 8) ())))))))
     (Apply (Int 0) ()))))))
