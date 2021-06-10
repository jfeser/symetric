((ops
  (Flip Reshape Flip Reshape
   (Id ((4 1 0 3 6 3 3 9 7 7 6 3 8 2 9 1 9 8 5 9 8 9 3 2) (2 3 4))) Cons
   (Int 1) Cons (Int 3) Vec (Int 8) (Int 2) Cons (Int 4) Vec (Int 6) 
   (Int 0)))
 (output (Tensor ((9 8 9 5 8 9 3 6 7 7 2 3 1 4 1 9 2 8 9 3 3 6 3 0) (4 6))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id ((4 1 0 3 6 3 3 9 7 7 6 3 8 2 9 1 9 8 5 9 8 9 3 2) (2 3 4)))
            ())
           (Apply Cons
            ((Apply (Int 1) ())
             (Apply Cons
              ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 8) ())))))))))
         (Apply (Int 2) ())))
       (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 6) ())))))))
     (Apply (Int 0) ()))))))
