((ops
  (Reshape Flip Flip Reshape
   (Id
    ((4 5 9 4 9 4 6 7 9 8 2 5 1 4 7 2 3 8 2 9 6 5 0 7 9 1 7 2 7 4 8 7 9 7 0 2
      7 3 4 4 7 5 0 6 7 3 2 4 9 7 4 9 9 6 8 7 8 9 5 0)
     (3 5 4)))
   Cons (Int 10) Cons (Int 1) Vec (Int 6) (Int 0) (Int 2) Cons (Int 3) Vec
   (Int 20)))
 (output
  (Tensor
   ((0 5 9 8 7 8 6 9 9 4 7 9 4 2 3 7 6 0 5 7 4 4 3 7 2 0 7 9 7 8 4 7 2 7 1 9
     7 0 5 6 9 2 8 3 2 7 4 1 5 2 8 9 7 6 4 9 4 9 5 4)
    (3 20))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((4 5 9 4 9 4 6 7 9 8 2 5 1 4 7 2 3 8 2 9 6 5 0 7 9 1 7 2 7 4 8
               7 9 7 0 2 7 3 4 4 7 5 0 6 7 3 2 4 9 7 4 9 9 6 8 7 8 9 5 0)
              (3 5 4)))
            ())
           (Apply Cons
            ((Apply (Int 10) ())
             (Apply Cons
              ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 6) ())))))))))
         (Apply (Int 0) ())))
       (Apply (Int 2) ())))
     (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 20) ()))))))))))
