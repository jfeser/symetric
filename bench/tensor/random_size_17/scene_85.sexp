((ops
  (Flip Reshape Reshape Flip
   (Id
    ((6 3 3 8 4 5 6 3 8 2 6 1 9 6 0 6 5 9 3 2 1 4 7 9 3 9 3 0 6 9 1 8 9 3 0 8
      5 6 4 6 3 1 5 2 3 2 0 9 4 2 5 1 6 7 8 4 7 5 9 0)
     (3 5 4)))
   (Int 2) Cons (Int 5) Cons (Int 12) Vec (Int 1) Cons (Int 3) Vec (Int 20)
   (Int 0)))
 (output
  (Tensor
   ((2 5 1 3 9 0 2 3 1 5 2 4 4 8 7 6 0 9 5 7 9 7 4 1 0 3 9 3 8 1 9 6 8 0 3 9
     6 4 6 5 8 3 3 6 3 6 5 4 1 6 2 8 6 0 6 9 2 3 9 5)
    (3 20))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((6 3 3 8 4 5 6 3 8 2 6 1 9 6 0 6 5 9 3 2 1 4 7 9 3 9 3 0 6 9 1
               8 9 3 0 8 5 6 4 6 3 1 5 2 3 2 0 9 4 2 5 1 6 7 8 4 7 5 9 0)
              (3 5 4)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons
          ((Apply (Int 5) ())
           (Apply Cons
            ((Apply (Int 12) ()) (Apply Vec ((Apply (Int 1) ())))))))))
       (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 20) ())))))))
     (Apply (Int 0) ()))))))
