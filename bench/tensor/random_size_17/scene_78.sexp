((ops
  (Flip Reshape Flip Reshape
   (Id
    ((7 3 9 5 9 3 4 6 7 9 0 3 2 1 0 8 2 7 8 5 3 6 3 3 5 2 9 8 1 3) (5 2 3)))
   Cons (Int 10) Vec (Int 3) (Int 0) Cons (Int 1) Cons (Int 6) Vec (Int 5)
   (Int 2)))
 (output
  (Tensor
   ((2 5 3 1 8 8 3 3 6 9 7 2 8 3 5 0 9 0 1 2 5 7 6 4 3 9 3 7 3 9) (1 6 5))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((7 3 9 5 9 3 4 6 7 9 0 3 2 1 0 8 2 7 8 5 3 6 3 3 5 2 9 8 1 3)
              (5 2 3)))
            ())
           (Apply Cons
            ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 3) ())))))))
         (Apply (Int 0) ())))
       (Apply Cons
        ((Apply (Int 1) ())
         (Apply Cons ((Apply (Int 6) ()) (Apply Vec ((Apply (Int 5) ())))))))))
     (Apply (Int 2) ()))))))
