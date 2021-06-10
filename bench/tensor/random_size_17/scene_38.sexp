((ops
  (Reshape Flip Flip Reshape
   (Id
    ((7 0 0 2 6 2 3 6 9 8 9 4 5 2 9 4 8 7 2 5 2 3 5 4 8 8 2 8 2 4 1 8 8 4 5 8
      6 3 4 9 7 2 5 5 4 9 7 6)
     (4 4 3)))
   Cons (Int 8) Cons (Int 1) Vec (Int 6) (Int 2) (Int 0) Cons (Int 4) Vec
   (Int 12)))
 (output
  (Tensor
   ((6 7 9 4 5 5 2 7 9 4 3 6 8 5 4 8 8 1 4 2 8 2 8 8 4 5 3 2 5 2 7 8 4 9 2 5
     4 9 8 9 6 3 2 6 2 0 0 7)
    (4 12))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((7 0 0 2 6 2 3 6 9 8 9 4 5 2 9 4 8 7 2 5 2 3 5 4 8 8 2 8 2 4 1
               8 8 4 5 8 6 3 4 9 7 2 5 5 4 9 7 6)
              (4 4 3)))
            ())
           (Apply Cons
            ((Apply (Int 8) ())
             (Apply Cons
              ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 6) ())))))))))
         (Apply (Int 2) ())))
       (Apply (Int 0) ())))
     (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 12) ()))))))))))
