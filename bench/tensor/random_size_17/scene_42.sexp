((ops
  (Flip Reshape Flip Reshape
   (Id
    ((6 6 9 0 7 0 4 6 7 1 5 6 4 4 5 8 6 2 3 1 9 6 6 1 8 3 3 2 7 1 9 3 5 3 4
      1)
     (3 4 3)))
   Cons (Int 9) Vec (Int 4) (Int 1) Cons (Int 2) Cons (Int 3) Vec (Int 6)
   (Int 0)))
 (output
  (Tensor
   ((2 6 1 6 6 9 2 3 3 8 3 9 1 7 1 4 3 5 0 9 6 6 6 4 0 7 6 5 1 7 8 5 4 4 1 3)
    (2 3 6))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((6 6 9 0 7 0 4 6 7 1 5 6 4 4 5 8 6 2 3 1 9 6 6 1 8 3 3 2 7 1 9
               3 5 3 4 1)
              (3 4 3)))
            ())
           (Apply Cons ((Apply (Int 9) ()) (Apply Vec ((Apply (Int 4) ())))))))
         (Apply (Int 1) ())))
       (Apply Cons
        ((Apply (Int 2) ())
         (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 6) ())))))))))
     (Apply (Int 0) ()))))))
