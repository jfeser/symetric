((ops
  (Flip Reshape Reshape Flip
   (Id
    ((6 2 7 2 6 8 0 5 9 4 4 6 3 7 3 3 2 6 0 9 8 1 2 4 1 7 4 2 3 1 2 6 3 1 9 7
      1 4 2 9 5 3 3 6 8 6 9 6)
     (4 3 4)))
   (Int 2) Cons (Int 8) Vec (Int 6) Cons (Int 16) Cons (Int 1) Vec (Int 3)
   (Int 0)))
 (output
  (Tensor
   ((9 6 8 3 5 6 1 6 3 9 2 4 9 1 3 1 3 7 1 6 2 2 4 7 2 1 8 6 2 4 3 9 0 3 3 7
     4 4 9 8 6 6 6 5 0 2 7 2)
    (16 1 3))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((6 2 7 2 6 8 0 5 9 4 4 6 3 7 3 3 2 6 0 9 8 1 2 4 1 7 4 2 3 1 2
               6 3 1 9 7 1 4 2 9 5 3 3 6 8 6 9 6)
              (4 3 4)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 6) ())))))))
       (Apply Cons
        ((Apply (Int 16) ())
         (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 3) ())))))))))
     (Apply (Int 0) ()))))))
