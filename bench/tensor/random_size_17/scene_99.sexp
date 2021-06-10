((ops
  (Flip Reshape Flip Reshape
   (Id
    ((0 2 2 3 7 4 4 2 7 8 9 5 9 7 5 6 1 6 7 8 8 2 4 9 9 9 0 6 6 8) (2 3 5)))
   Cons (Int 1) Cons (Int 5) Vec (Int 6) (Int 2) Cons (Int 3) Vec (Int 10)
   (Int 0)))
 (output
  (Tensor
   ((2 8 8 7 8 6 6 0 9 9 2 4 6 1 6 5 7 9 9 4 4 7 3 2 2 0 5 9 8 7) (3 10))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((0 2 2 3 7 4 4 2 7 8 9 5 9 7 5 6 1 6 7 8 8 2 4 9 9 9 0 6 6 8)
              (2 3 5)))
            ())
           (Apply Cons
            ((Apply (Int 1) ())
             (Apply Cons
              ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 6) ())))))))))
         (Apply (Int 2) ())))
       (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 10) ())))))))
     (Apply (Int 0) ()))))))
