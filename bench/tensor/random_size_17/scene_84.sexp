((ops
  (Reshape Reshape Flip Flip
   (Id
    ((2 1 8 1 5 8 9 7 1 2 6 0 7 0 3 9 3 5 5 4 2 0 6 5 9 9 2 8 6 6 4 7 0 4 1 6
      5 4 5 3 3 8 4 1 2 4 0 4 4 9 9 9 4 0 9 1 6 8 8 5 2 2 4 1 7 3 0 4 4 1 8 0
      5 5 9 5 3 6 0 0 1 1 1 9 0 4 6 2 8 7 1 2 6 8 8 7 0 9 5 7)
     (5 4 5)))
   (Int 0) (Int 2) Cons (Int 5) Cons (Int 20) Vec (Int 1) Cons (Int 4) Vec
   (Int 25)))
 (output
  (Tensor
   ((0 9 1 1 1 7 8 2 6 4 8 8 6 2 1 7 5 9 0 7 7 1 4 2 2 1 4 4 0 3 9 5 5 0 8 0
     0 6 3 5 2 1 4 8 3 9 4 4 0 4 9 0 4 9 9 5 8 8 6 1 9 5 6 0 2 6 6 8 2 9 1 4
     0 7 4 3 5 4 5 6 5 1 8 1 2 2 1 7 9 8 3 0 7 0 6 4 5 5 3 9)
    (4 25))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((2 1 8 1 5 8 9 7 1 2 6 0 7 0 3 9 3 5 5 4 2 0 6 5 9 9 2 8 6 6 4
               7 0 4 1 6 5 4 5 3 3 8 4 1 2 4 0 4 4 9 9 9 4 0 9 1 6 8 8 5 2 2
               4 1 7 3 0 4 4 1 8 0 5 5 9 5 3 6 0 0 1 1 1 9 0 4 6 2 8 7 1 2 6
               8 8 7 0 9 5 7)
              (5 4 5)))
            ())
           (Apply (Int 0) ())))
         (Apply (Int 2) ())))
       (Apply Cons
        ((Apply (Int 5) ())
         (Apply Cons ((Apply (Int 20) ()) (Apply Vec ((Apply (Int 1) ())))))))))
     (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 25) ()))))))))))
