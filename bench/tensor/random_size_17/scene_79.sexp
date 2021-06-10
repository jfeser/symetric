((ops
  (Flip Reshape Reshape Flip
   (Id
    ((7 6 5 8 5 0 4 9 9 0 4 1 0 7 8 1 0 3 9 7 4 5 1 9 4 6 7 0 2 5 2 2 2 4 2 7
      2 9 4 1 8 7 9 1 2)
     (3 5 3)))
   (Int 2) Cons (Int 15) Vec (Int 3) Cons (Int 9) Cons (Int 5) Vec (Int 1)
   (Int 0)))
 (output
  (Tensor
   ((8 1 2 1 9 4 4 9 2 7 2 2 2 7 2 6 4 5 2 0 9 9 1 5 7 3 0 1 4 7 4 0 8 7 0 8
     9 9 4 1 5 6 7 0 5)
    (9 5 1))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((7 6 5 8 5 0 4 9 9 0 4 1 0 7 8 1 0 3 9 7 4 5 1 9 4 6 7 0 2 5 2
               2 2 4 2 7 2 9 4 1 8 7 9 1 2)
              (3 5 3)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 15) ()) (Apply Vec ((Apply (Int 3) ())))))))
       (Apply Cons
        ((Apply (Int 9) ())
         (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 1) ())))))))))
     (Apply (Int 0) ()))))))
