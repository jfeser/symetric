((ops
  (Reshape Reshape Flip Flip
   (Id
    ((0 5 3 6 2 8 1 7 3 8 9 5 5 6 4 6 3 0 0 5 7 9 0 4 6 9 2 0 8 6 3 3 5 9 8 5
      2 6 7 7 5 8 8 2 4 7 6 6 2 6 9 6 9 8 6 0 8 1 0 8 4 2 7 2 3 9 2 7 7 4 2 5
      3 7 4 8 3 5 4 4)
     (4 5 4)))
   (Int 2) (Int 0) Cons (Int 10) Cons (Int 8) Vec (Int 1) Cons (Int 4) Vec
   (Int 20)))
 (output
  (Tensor
   ((2 7 2 4 7 2 9 3 5 2 4 7 8 4 7 3 4 4 5 3 2 8 8 5 6 6 7 4 6 9 6 2 0 6 8 9
     8 0 1 8 4 0 9 7 0 2 9 6 3 3 6 8 5 8 9 5 7 7 6 2 6 3 5 0 7 1 8 2 5 9 8 3
     6 4 6 5 5 0 0 3)
    (4 20))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((0 5 3 6 2 8 1 7 3 8 9 5 5 6 4 6 3 0 0 5 7 9 0 4 6 9 2 0 8 6 3
               3 5 9 8 5 2 6 7 7 5 8 8 2 4 7 6 6 2 6 9 6 9 8 6 0 8 1 0 8 4 2
               7 2 3 9 2 7 7 4 2 5 3 7 4 8 3 5 4 4)
              (4 5 4)))
            ())
           (Apply (Int 2) ())))
         (Apply (Int 0) ())))
       (Apply Cons
        ((Apply (Int 10) ())
         (Apply Cons ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 1) ())))))))))
     (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 20) ()))))))))))
