((ops
  (Reshape Flip Reshape Flip
   (Id
    ((5 8 0 8 5 3 3 2 8 2 2 1 3 8 6 9 4 7 2 7 1 7 1 0 5 1 8 8 1 0 0 6 0 9 7 1
      1 4 5 5 4 4 5 2 4 8 0 7 7 4 7 3 3 1 8 6 3 7 2 6)
     (4 3 5)))
   (Int 0) Cons (Int 4) Cons (Int 5) Vec (Int 3) (Int 1) Cons (Int 2) Vec
   (Int 30)))
 (output
  (Tensor
   ((7 2 6 8 6 3 3 3 1 7 4 7 8 0 7 5 2 4 5 4 4 1 4 5 9 7 1 0 6 0 8 1 0 5 1 8
     7 1 0 2 7 1 9 4 7 3 8 6 2 2 1 3 2 8 8 5 3 5 8 0)
    (2 30))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((5 8 0 8 5 3 3 2 8 2 2 1 3 8 6 9 4 7 2 7 1 7 1 0 5 1 8 8 1 0 0
               6 0 9 7 1 1 4 5 5 4 4 5 2 4 8 0 7 7 4 7 3 3 1 8 6 3 7 2 6)
              (4 3 5)))
            ())
           (Apply (Int 0) ())))
         (Apply Cons
          ((Apply (Int 4) ())
           (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 3) ())))))))))
       (Apply (Int 1) ())))
     (Apply Cons ((Apply (Int 2) ()) (Apply Vec ((Apply (Int 30) ()))))))))))
