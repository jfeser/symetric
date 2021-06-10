((ops
  (Flip Reshape Reshape Flip
   (Id
    ((9 7 1 8 1 0 0 7 0 0 3 4 7 7 3 1 1 1 8 7 2 8 5 0 3 5 9 2 1 0 3 4 1 0 7 1
      7 7 9 3)
     (5 2 4)))
   (Int 2) Cons (Int 1) Cons (Int 8) Vec (Int 5) Cons (Int 4) Vec (Int 10)
   (Int 0)))
 (output
  (Tensor
   ((0 1 1 7 0 1 3 9 7 7 0 5 8 2 2 9 5 3 4 3 0 0 1 3 7 7 7 8 1 1 8 1 7 9 7 0
     0 1 4 3)
    (4 10))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((9 7 1 8 1 0 0 7 0 0 3 4 7 7 3 1 1 1 8 7 2 8 5 0 3 5 9 2 1 0 3
               4 1 0 7 1 7 7 9 3)
              (5 2 4)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons
          ((Apply (Int 1) ())
           (Apply Cons ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 5) ())))))))))
       (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 10) ())))))))
     (Apply (Int 0) ()))))))
