((ops
  (Flip Reshape Reshape Flip
   (Id
    ((9 8 6 9 2 9 8 2 6 9 8 2 3 4 4 4 1 1 8 6 0 9 5 8 3 5 2 6 3 6 0 0 6 7 9 1
      9 5 5 2 2 0 9 7 7 9 7 2)
     (4 3 4)))
   (Int 2) Cons (Int 4) Vec (Int 12) Cons (Int 8) Cons (Int 6) Vec (Int 1)
   (Int 0)))
 (output
  (Tensor
   ((0 2 2 7 9 7 2 5 5 9 7 9 6 3 1 9 7 6 6 2 5 3 0 0 1 1 8 5 9 0 4 4 4 3 6 8
     9 2 2 8 9 6 9 6 8 9 2 8)
    (8 6 1))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((9 8 6 9 2 9 8 2 6 9 8 2 3 4 4 4 1 1 8 6 0 9 5 8 3 5 2 6 3 6 0
               0 6 7 9 1 9 5 5 2 2 0 9 7 7 9 7 2)
              (4 3 4)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 12) ())))))))
       (Apply Cons
        ((Apply (Int 8) ())
         (Apply Cons ((Apply (Int 6) ()) (Apply Vec ((Apply (Int 1) ())))))))))
     (Apply (Int 0) ()))))))
