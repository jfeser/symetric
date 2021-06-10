((ops
  (Reshape Reshape Flip Flip
   (Id
    ((7 2 7 4 8 0 5 1 1 3 6 8 1 0 2 9 6 3 4 1 7 8 0 8 5 6 1 4 8 4 9 1 0 8 7 9
      6 1 7 3 2 2 9 4 1 8 9 9 6 2 0 6 1 3 6 2 9 1 9 2 5 4 2 6 2 3 0 1 3 0 1 8
      8 3 7 2 8 9 1 2 6 3 6 2 6 2 9 1 2 7 8 4 5 4 4 6 7 6 6 5)
     (4 5 5)))
   (Int 0) (Int 1) Cons (Int 5) Cons (Int 2) Vec (Int 10) Cons (Int 25) Vec
   (Int 4)))
 (output
  (Tensor
   ((6 7 6 6 5 8 4 5 4 4 2 9 1 2 7 6 3 6 2 6 2 8 9 1 2 1 8 8 3 7 3 0 1 3 0 5
     4 2 6 2 2 9 1 9 2 0 6 1 3 6 8 9 9 6 2 2 2 9 4 1 9 6 1 7 3 9 1 0 8 7 6 1
     4 8 4 7 8 0 8 5 9 6 3 4 1 6 8 1 0 2 0 5 1 1 3 7 2 7 4 8)
    (25 4))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((7 2 7 4 8 0 5 1 1 3 6 8 1 0 2 9 6 3 4 1 7 8 0 8 5 6 1 4 8 4 9
               1 0 8 7 9 6 1 7 3 2 2 9 4 1 8 9 9 6 2 0 6 1 3 6 2 9 1 9 2 5 4
               2 6 2 3 0 1 3 0 1 8 8 3 7 2 8 9 1 2 6 3 6 2 6 2 9 1 2 7 8 4 5
               4 4 6 7 6 6 5)
              (4 5 5)))
            ())
           (Apply (Int 0) ())))
         (Apply (Int 1) ())))
       (Apply Cons
        ((Apply (Int 5) ())
         (Apply Cons ((Apply (Int 2) ()) (Apply Vec ((Apply (Int 10) ())))))))))
     (Apply Cons ((Apply (Int 25) ()) (Apply Vec ((Apply (Int 4) ()))))))))))
