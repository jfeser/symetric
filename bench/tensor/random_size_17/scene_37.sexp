((ops
  (Flip Reshape Flip Reshape Flip
   (Id ((1 7 6 0 8 1 8 3 7 0 9 9 3 5 1 2 3 6 8 6 2 8 8 3) (4 3 2))) (Int 2)
   Cons (Int 4) Vec (Int 6) (Int 0) Cons (Int 8) Vec (Int 3) (Int 1)))
 (output (Tensor ((8 8 6 8 3 2 2 3 5 3 6 1 0 8 3 9 9 7 0 1 7 8 1 6) (8 3))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply Flip
            ((Apply
              (Id
               ((1 7 6 0 8 1 8 3 7 0 9 9 3 5 1 2 3 6 8 6 2 8 8 3) (4 3 2)))
              ())
             (Apply (Int 2) ())))
           (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 6) ())))))))
         (Apply (Int 0) ())))
       (Apply Cons ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 3) ())))))))
     (Apply (Int 1) ()))))))
