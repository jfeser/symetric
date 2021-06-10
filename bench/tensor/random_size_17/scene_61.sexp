((ops
  (Reshape Flip Reshape Flip
   (Id
    ((7 3 0 7 4 7 8 0 0 0 2 9 9 7 5 6 0 9 4 9 0 6 7 9 6 1 4 3 9 3 9 1 1 6 3 3
      8 5 8 2 6 4 0 7 4 5 7 7 7 6 7 9 2 3 1 3 2 5 7 7)
     (3 5 4)))
   (Int 0) Cons (Int 4) Cons (Int 5) Vec (Int 3) (Int 1) Cons (Int 10) Vec
   (Int 6)))
 (output
  (Tensor
   ((2 3 1 6 7 9 7 7 7 7 4 5 6 4 0 3 9 3 6 1 4 6 7 9 7 7 0 3 2 5 0 7 4 2 7 3
     8 5 8 6 3 3 9 1 1 9 4 9 5 6 0 9 9 7 0 0 2 7 8 0)
    (10 6))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((7 3 0 7 4 7 8 0 0 0 2 9 9 7 5 6 0 9 4 9 0 6 7 9 6 1 4 3 9 3 9
               1 1 6 3 3 8 5 8 2 6 4 0 7 4 5 7 7 7 6 7 9 2 3 1 3 2 5 7 7)
              (3 5 4)))
            ())
           (Apply (Int 0) ())))
         (Apply Cons
          ((Apply (Int 4) ())
           (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 3) ())))))))))
       (Apply (Int 1) ())))
     (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 6) ()))))))))))
