((ops
  (Reshape Flip Flip Reshape
   (Id
    ((7 3 1 2 1 0 2 5 6 9 3 5 8 9 9 8 2 1 9 7 8 3 4 5 9 3 5 4 8 4 9 6 8 9 7 9
      5 3 2 7 6 3 9 3 6 2 7 1 9 7 5 2 4 7 5 8 3 1 6 3 3 2 6 9 7 0 3 8 5 8 6 0
      7 7 9 2 1 6 7 5)
     (5 4 4)))
   Cons (Int 4) Cons (Int 1) Vec (Int 20) (Int 2) (Int 0) Cons (Int 10) Vec
   (Int 8)))
 (output
  (Tensor
   ((5 7 6 1 2 9 7 7 0 6 8 5 8 3 0 7 9 6 2 3 3 6 1 3 8 5 7 4 2 5 7 9 1 7 2 6
     3 9 3 6 7 2 3 5 9 7 9 8 6 9 4 8 4 5 3 9 5 4 3 8 7 9 1 2 8 9 9 8 5 3 9 6
     5 2 0 1 2 1 3 7)
    (10 8))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((7 3 1 2 1 0 2 5 6 9 3 5 8 9 9 8 2 1 9 7 8 3 4 5 9 3 5 4 8 4 9
               6 8 9 7 9 5 3 2 7 6 3 9 3 6 2 7 1 9 7 5 2 4 7 5 8 3 1 6 3 3 2
               6 9 7 0 3 8 5 8 6 0 7 7 9 2 1 6 7 5)
              (5 4 4)))
            ())
           (Apply Cons
            ((Apply (Int 4) ())
             (Apply Cons
              ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 20) ())))))))))
         (Apply (Int 2) ())))
       (Apply (Int 0) ())))
     (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 8) ()))))))))))
