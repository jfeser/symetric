((ops
  (Flip Reshape Flip Reshape
   (Id
    ((3 7 8 9 5 6 9 8 1 8 0 6 8 6 8 1 3 5 9 7 4 5 4 9 6 3 0 5 4 1 4 1 6 2 6
      5)
     (3 3 4)))
   Cons (Int 4) Vec (Int 9) (Int 1) Cons (Int 2) Cons (Int 6) Vec (Int 3)
   (Int 0)))
 (output
  (Tensor
   ((0 3 6 9 4 5 4 7 9 5 6 2 6 1 4 1 4 5 1 8 9 6 5 9 8 7 3 5 3 1 8 6 8 6 0 8)
    (2 6 3))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((3 7 8 9 5 6 9 8 1 8 0 6 8 6 8 1 3 5 9 7 4 5 4 9 6 3 0 5 4 1 4
               1 6 2 6 5)
              (3 3 4)))
            ())
           (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 9) ())))))))
         (Apply (Int 1) ())))
       (Apply Cons
        ((Apply (Int 2) ())
         (Apply Cons ((Apply (Int 6) ()) (Apply Vec ((Apply (Int 3) ())))))))))
     (Apply (Int 0) ()))))))
