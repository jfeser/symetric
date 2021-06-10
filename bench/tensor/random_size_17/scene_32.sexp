((ops
  (Flip Reshape Flip Reshape Flip
   (Id ((2 3 9 2 7 8 9 0 0 9 3 9 6 0 9 9 3 7 9 3 3 2 9 5) (4 2 3))) (Int 2)
   Cons (Int 4) Vec (Int 6) (Int 1) Cons (Int 8) Vec (Int 3) (Int 0)))
 (output (Tensor ((9 3 3 2 9 5 6 0 9 9 3 7 9 0 0 9 3 9 2 3 9 2 7 8) (8 3))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply Flip
            ((Apply
              (Id
               ((2 3 9 2 7 8 9 0 0 9 3 9 6 0 9 9 3 7 9 3 3 2 9 5) (4 2 3)))
              ())
             (Apply (Int 2) ())))
           (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 6) ())))))))
         (Apply (Int 1) ())))
       (Apply Cons ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 3) ())))))))
     (Apply (Int 0) ()))))))
