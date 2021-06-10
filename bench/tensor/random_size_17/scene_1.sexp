((ops
  (Flip Reshape Flip Reshape
   (Id
    ((6 2 5 0 4 3 2 4 0 5 0 8 9 2 7 6 3 6 3 2 0 0 1 4 4 3 3 4 1 0 2 0 7 5 7 6
      9 7 6 5 0 2 2 4 3 4 0 4 4 0 5 8 7 4 4 4 6 9 0 7)
     (4 3 5)))
   Cons (Int 12) Vec (Int 5) (Int 0) Cons (Int 3) Cons (Int 2) Vec (Int 10)
   (Int 1)))
 (output
  (Tensor
   ((4 0 4 4 0 0 2 2 4 3 4 6 9 0 7 5 8 7 4 4 3 3 4 1 0 0 0 1 4 4 6 9 7 6 5 2
     0 7 5 7 3 2 4 0 5 6 2 5 0 4 6 3 6 3 2 0 8 9 2 7)
    (3 2 10))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((6 2 5 0 4 3 2 4 0 5 0 8 9 2 7 6 3 6 3 2 0 0 1 4 4 3 3 4 1 0 2
               0 7 5 7 6 9 7 6 5 0 2 2 4 3 4 0 4 4 0 5 8 7 4 4 4 6 9 0 7)
              (4 3 5)))
            ())
           (Apply Cons
            ((Apply (Int 12) ()) (Apply Vec ((Apply (Int 5) ())))))))
         (Apply (Int 0) ())))
       (Apply Cons
        ((Apply (Int 3) ())
         (Apply Cons ((Apply (Int 2) ()) (Apply Vec ((Apply (Int 10) ())))))))))
     (Apply (Int 1) ()))))))
