((ops
  (Reshape Flip Reshape Flip
   (Id
    ((6 7 3 2 1 9 3 8 4 4 5 3 3 0 1 2 7 0 7 3 1 8 0 9 8 8 4 5 4 6 1 6 7 8 0 9
      6 5 3 3 7 6 8 5 4 7 7 1 0 8 4 1 2 6 2 6 1 6 5 4)
     (5 3 4)))
   (Int 0) Cons (Int 6) Cons (Int 5) Vec (Int 2) (Int 1) Cons (Int 3) Vec
   (Int 20)))
 (output
  (Tensor
   ((1 6 2 6 2 6 4 1 0 8 8 5 7 6 3 3 6 5 5 4 4 6 4 5 8 8 7 1 4 7 1 2 3 0 0 9
     7 8 1 6 6 7 0 9 1 8 7 3 7 0 5 3 4 4 3 8 1 9 3 2)
    (3 20))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((6 7 3 2 1 9 3 8 4 4 5 3 3 0 1 2 7 0 7 3 1 8 0 9 8 8 4 5 4 6 1
               6 7 8 0 9 6 5 3 3 7 6 8 5 4 7 7 1 0 8 4 1 2 6 2 6 1 6 5 4)
              (5 3 4)))
            ())
           (Apply (Int 0) ())))
         (Apply Cons
          ((Apply (Int 6) ())
           (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 2) ())))))))))
       (Apply (Int 1) ())))
     (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 20) ()))))))))))
