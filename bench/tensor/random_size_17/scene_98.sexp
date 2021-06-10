((ops
  (Reshape Flip Reshape Flip
   (Id
    ((6 6 2 2 8 8 1 9 2 4 9 2 0 3 7 0 4 7 7 1 0 1 6 5 2 1 0 1 6 4 8 0 2 0 3 6
      9 6 2 5 5 5 0 0 4 8 9 3 3 5 1 2 8 8 2 3 6 9 1 7 3 8 2 0 5 8 0 5 0 5 4 1
      5 9 5)
     (5 5 3)))
   (Int 2) Cons (Int 5) Cons (Int 15) Vec (Int 1) (Int 0) Cons (Int 3) Vec
   (Int 25)))
 (output
  (Tensor
   ((2 8 3 8 5 0 0 5 0 1 4 5 5 9 5 3 9 8 1 5 3 8 8 2 6 3 2 7 1 9 2 0 8 6 3 0
     2 6 9 5 5 5 4 0 0 7 4 0 0 1 7 5 6 1 0 1 2 4 6 1 2 6 6 8 8 2 2 9 1 2 9 4
     7 3 0)
    (3 25))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((6 6 2 2 8 8 1 9 2 4 9 2 0 3 7 0 4 7 7 1 0 1 6 5 2 1 0 1 6 4 8
               0 2 0 3 6 9 6 2 5 5 5 0 0 4 8 9 3 3 5 1 2 8 8 2 3 6 9 1 7 3 8
               2 0 5 8 0 5 0 5 4 1 5 9 5)
              (5 5 3)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons
          ((Apply (Int 5) ())
           (Apply Cons
            ((Apply (Int 15) ()) (Apply Vec ((Apply (Int 1) ())))))))))
       (Apply (Int 0) ())))
     (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 25) ()))))))))))
