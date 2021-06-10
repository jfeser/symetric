((ops
  (Flip Reshape Reshape Flip
   (Id
    ((5 1 5 2 4 7 3 3 6 3 8 4 9 0 1 5 3 7 5 1 2 1 6 6 2 7 3 5 3 9 1 5 3 4 8 4
      0 3 1 7 2 4 6 0 0 8 7 0 9 5 6 3 6 3 3 4 2 5 3 9 6 4 9 0 1 0 4 6 6 4 1 3
      0 2 5)
     (5 3 5)))
   (Int 2) Cons (Int 25) Vec (Int 3) Cons (Int 5) Cons (Int 15) Vec (Int 1)
   (Int 0)))
 (output
  (Tensor
   ((1 0 9 4 6 4 6 6 4 0 5 2 0 3 1 5 9 0 7 8 3 3 6 3 6 9 3 5 2 4 8 4 3 5 1 7
     1 3 0 4 0 0 6 4 2 1 5 7 3 5 2 6 6 1 2 9 3 5 3 7 4 2 5 1 5 3 6 3 3 7 1 0
     9 4 8)
    (5 15 1))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((5 1 5 2 4 7 3 3 6 3 8 4 9 0 1 5 3 7 5 1 2 1 6 6 2 7 3 5 3 9 1
               5 3 4 8 4 0 3 1 7 2 4 6 0 0 8 7 0 9 5 6 3 6 3 3 4 2 5 3 9 6 4
               9 0 1 0 4 6 6 4 1 3 0 2 5)
              (5 3 5)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 25) ()) (Apply Vec ((Apply (Int 3) ())))))))
       (Apply Cons
        ((Apply (Int 5) ())
         (Apply Cons ((Apply (Int 15) ()) (Apply Vec ((Apply (Int 1) ())))))))))
     (Apply (Int 0) ()))))))
