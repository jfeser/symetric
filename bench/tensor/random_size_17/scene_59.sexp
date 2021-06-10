((ops
  (Flip Reshape Reshape Flip
   (Id
    ((2 6 7 5 2 5 5 1 2 3 9 9 3 8 9 4 9 8 5 2 7 3 0 1 6 0 8 7 4 1 7 6 9 7 7
      9)
     (3 3 4)))
   (Int 2) Cons (Int 4) Vec (Int 9) Cons (Int 12) Cons (Int 3) Vec (Int 1)
   (Int 0)))
 (output
  (Tensor
   ((7 7 9 1 4 9 6 6 7 7 8 0 0 3 7 8 9 1 3 2 5 4 9 8 9 3 2 5 2 9 2 1 5 5 7 6)
    (12 3 1))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((2 6 7 5 2 5 5 1 2 3 9 9 3 8 9 4 9 8 5 2 7 3 0 1 6 0 8 7 4 1 7
               6 9 7 7 9)
              (3 3 4)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 9) ())))))))
       (Apply Cons
        ((Apply (Int 12) ())
         (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 1) ())))))))))
     (Apply (Int 0) ()))))))
