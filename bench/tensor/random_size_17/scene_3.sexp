((ops
  (Flip Reshape Reshape Flip
   (Id
    ((8 1 5 9 8 2 6 3 3 0 0 1 4 7 9 5 1 8 3 6 2 9 1 4 0 2 8 0 3 5) (5 3 2)))
   (Int 2) Cons (Int 3) Vec (Int 10) Cons (Int 6) Cons (Int 5) Vec (Int 1)
   (Int 0)))
 (output
  (Tensor
   ((0 0 8 5 3 9 2 4 1 2 9 8 1 6 3 1 0 7 4 5 8 3 6 0 3 1 8 9 5 2) (6 5 1))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((8 1 5 9 8 2 6 3 3 0 0 1 4 7 9 5 1 8 3 6 2 9 1 4 0 2 8 0 3 5)
              (5 3 2)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 10) ())))))))
       (Apply Cons
        ((Apply (Int 6) ())
         (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 1) ())))))))))
     (Apply (Int 0) ()))))))
