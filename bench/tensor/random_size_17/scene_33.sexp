((ops
  (Reshape Reshape Flip Flip
   (Id
    ((3 3 8 8 7 7 5 9 2 3 2 4 4 4 4 4 5 9 2 6 8 8 4 0 8 1 5 0 7 7 1 8 4 8 5
      0)
     (3 3 4)))
   (Int 0) (Int 2) Cons (Int 3) Cons (Int 1) Vec (Int 12) Cons (Int 4) Vec
   (Int 9)))
 (output
  (Tensor
   ((0 5 1 8 8 1 7 7 0 5 8 4 4 4 4 4 6 2 9 5 0 4 8 8 8 8 3 3 9 5 7 7 4 2 3 2)
    (4 9))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((3 3 8 8 7 7 5 9 2 3 2 4 4 4 4 4 5 9 2 6 8 8 4 0 8 1 5 0 7 7 1
               8 4 8 5 0)
              (3 3 4)))
            ())
           (Apply (Int 0) ())))
         (Apply (Int 2) ())))
       (Apply Cons
        ((Apply (Int 3) ())
         (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 12) ())))))))))
     (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 9) ()))))))))))
