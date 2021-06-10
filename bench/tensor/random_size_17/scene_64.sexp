((ops
  (Reshape Flip Reshape Flip
   (Id
    ((0 0 5 7 9 9 4 2 5 7 0 5 1 2 1 1 6 1 9 0 1 0 8 8 2 1 8 3 1 2 1 0 9 1 6 1
      4 1 6 7 5 7 3 8 4 1 3 0 7 3 8 7 6 9 7 4 3 3 3 2 5 7 7 9 2 4 4 7 7 9 1 4
      1 4 9)
     (5 3 5)))
   (Int 2) Cons (Int 15) Cons (Int 1) Vec (Int 5) (Int 0) Cons (Int 25) Vec
   (Int 3)))
 (output
  (Tensor
   ((9 4 1 4 1 9 7 7 4 4 2 9 7 7 5 2 3 3 3 4 7 9 6 7 8 3 7 0 3 1 4 8 3 7 5 7
     6 1 4 1 6 1 9 0 1 2 1 3 8 1 2 8 8 0 1 0 9 1 6 1 1 2 1 5 0 7 5 2 4 9 9 7
     5 0 0)
    (25 3))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((0 0 5 7 9 9 4 2 5 7 0 5 1 2 1 1 6 1 9 0 1 0 8 8 2 1 8 3 1 2 1
               0 9 1 6 1 4 1 6 7 5 7 3 8 4 1 3 0 7 3 8 7 6 9 7 4 3 3 3 2 5 7
               7 9 2 4 4 7 7 9 1 4 1 4 9)
              (5 3 5)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons
          ((Apply (Int 15) ())
           (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 5) ())))))))))
       (Apply (Int 0) ())))
     (Apply Cons ((Apply (Int 25) ()) (Apply Vec ((Apply (Int 3) ()))))))))))
