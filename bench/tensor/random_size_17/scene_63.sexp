((ops
  (Flip Reshape Flip Reshape
   (Id
    ((1 3 5 4 0 1 8 1 3 5 7 4 0 3 4 2 1 9 4 6 0 2 7 1 8 8 0 1 9 0 6 7 1 1 2 8
      6 2 3 7 6 5 3 7 9 4 3 1 0 7 6 9 8 5 5 2 3 3 5 4)
     (3 4 5)))
   Cons (Int 5) Vec (Int 12) (Int 0) Cons (Int 2) Cons (Int 10) Vec (Int 3)
   (Int 1)))
 (output
  (Tensor
   ((1 9 0 8 8 0 4 3 1 3 7 9 7 6 5 6 2 3 3 5 4 5 2 3 9 8 5 0 7 6 5 7 4 8 1 3
     4 0 1 1 3 5 2 7 1 4 6 0 2 1 9 0 3 4 1 2 8 6 7 1)
    (2 10 3))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((1 3 5 4 0 1 8 1 3 5 7 4 0 3 4 2 1 9 4 6 0 2 7 1 8 8 0 1 9 0 6
               7 1 1 2 8 6 2 3 7 6 5 3 7 9 4 3 1 0 7 6 9 8 5 5 2 3 3 5 4)
              (3 4 5)))
            ())
           (Apply Cons
            ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 12) ())))))))
         (Apply (Int 0) ())))
       (Apply Cons
        ((Apply (Int 2) ())
         (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 3) ())))))))))
     (Apply (Int 1) ()))))))
