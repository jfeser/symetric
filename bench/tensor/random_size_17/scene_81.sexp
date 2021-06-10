((ops
  (Flip Reshape Flip Reshape
   (Id ((3 2 0 9 1 0 3 1 9 5 7 4 3 5 2 6 9 7 0 0 3 1 2 0) (2 3 4))) Cons
   (Int 1) Cons (Int 4) Vec (Int 6) (Int 2) Cons (Int 8) Vec (Int 3) 
   (Int 0)))
 (output (Tensor ((3 0 0 0 2 1 2 5 3 7 9 6 9 1 3 4 7 5 0 2 3 0 1 9) (8 3))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id ((3 2 0 9 1 0 3 1 9 5 7 4 3 5 2 6 9 7 0 0 3 1 2 0) (2 3 4)))
            ())
           (Apply Cons
            ((Apply (Int 1) ())
             (Apply Cons
              ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 6) ())))))))))
         (Apply (Int 2) ())))
       (Apply Cons ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 3) ())))))))
     (Apply (Int 0) ()))))))
