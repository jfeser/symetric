((ops
  (Flip Reshape Flip Reshape
   (Id
    ((8 3 1 6 3 7 8 3 0 4 1 7 1 1 9 8 6 0 2 4 8 5 0 3 2 7 3 4 3 8 3 4 0 8 2 2
      1 5 0 8 0 6 0 8 9 3 1 0 6 8 9 2 6 3 2 0 5 2 8 5)
     (4 3 5)))
   Cons (Int 20) Cons (Int 1) Vec (Int 3) (Int 2) Cons (Int 12) Vec (Int 5)
   (Int 0)))
 (output
  (Tensor
   ((0 2 5 8 2 6 3 6 2 5 0 1 3 9 8 0 8 9 8 0 8 0 5 1 6 0 4 3 2 2 7 2 8 3 4 2
     3 0 5 3 0 6 8 8 4 1 4 9 1 1 6 0 3 8 7 1 3 8 7 3)
    (12 5))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((8 3 1 6 3 7 8 3 0 4 1 7 1 1 9 8 6 0 2 4 8 5 0 3 2 7 3 4 3 8 3
               4 0 8 2 2 1 5 0 8 0 6 0 8 9 3 1 0 6 8 9 2 6 3 2 0 5 2 8 5)
              (4 3 5)))
            ())
           (Apply Cons
            ((Apply (Int 20) ())
             (Apply Cons
              ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 3) ())))))))))
         (Apply (Int 2) ())))
       (Apply Cons ((Apply (Int 12) ()) (Apply Vec ((Apply (Int 5) ())))))))
     (Apply (Int 0) ()))))))
