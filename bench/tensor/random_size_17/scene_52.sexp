((ops
  (Flip Reshape Reshape Flip
   (Id
    ((1 7 0 3 8 3 1 5 2 2 5 9 5 4 6 6 2 8 0 2 0 1 2 6 9 8 4 1 9 1) (5 2 3)))
   (Int 2) Cons (Int 3) Vec (Int 10) Cons (Int 5) Cons (Int 6) Vec (Int 1)
   (Int 0)))
 (output
  (Tensor
   ((4 8 9 1 9 1 0 2 0 6 2 1 6 4 5 8 2 6 2 5 1 9 5 2 0 7 1 3 8 3) (5 6 1))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((1 7 0 3 8 3 1 5 2 2 5 9 5 4 6 6 2 8 0 2 0 1 2 6 9 8 4 1 9 1)
              (5 2 3)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 10) ())))))))
       (Apply Cons
        ((Apply (Int 5) ())
         (Apply Cons ((Apply (Int 6) ()) (Apply Vec ((Apply (Int 1) ())))))))))
     (Apply (Int 0) ()))))))
