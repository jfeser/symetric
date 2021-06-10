((ops
  (Flip Reshape Reshape Flip
   (Id
    ((1 6 3 6 1 3 6 4 7 0 3 0 5 3 7 0 5 7 1 8 7 8 3 9 8 3 1 6 0 1 0 9 7 4 1
      3)
     (3 4 3)))
   (Int 2) Cons (Int 9) Cons (Int 1) Vec (Int 4) Cons (Int 12) Vec (Int 3)
   (Int 0)))
 (output
  (Tensor
   ((3 1 4 7 9 0 1 0 6 1 3 8 9 3 8 7 8 1 7 5 0 7 3 5 0 3 0 7 4 6 3 1 6 3 6 1)
    (12 3))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((1 6 3 6 1 3 6 4 7 0 3 0 5 3 7 0 5 7 1 8 7 8 3 9 8 3 1 6 0 1 0
               9 7 4 1 3)
              (3 4 3)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons
          ((Apply (Int 9) ())
           (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 4) ())))))))))
       (Apply Cons ((Apply (Int 12) ()) (Apply Vec ((Apply (Int 3) ())))))))
     (Apply (Int 0) ()))))))
