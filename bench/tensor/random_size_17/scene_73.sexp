((ops
  (Flip Reshape Flip Reshape
   (Id
    ((5 2 8 9 9 2 2 9 3 6 6 7 3 9 4 9 1 6 8 1 3 3 3 0 9 3 0 1 3 9 4 3 3 4 2 5
      1 7 2 2 4 6 8 3 1 8 0 4 3 3 5 6 2 9 1 7 5 0 1 9)
     (5 4 3)))
   Cons (Int 20) Vec (Int 3) (Int 1) Cons (Int 6) Cons (Int 2) Vec (Int 5)
   (Int 0)))
 (output
  (Tensor
   ((3 9 2 6 5 7 1 9 1 0 4 2 1 3 8 4 0 8 5 3 3 3 4 5 2 4 2 7 1 6 8 0 3 3 0 3
     9 9 3 1 6 6 4 9 3 6 1 9 3 1 8 2 5 2 9 9 3 9 2 7)
    (6 2 5))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((5 2 8 9 9 2 2 9 3 6 6 7 3 9 4 9 1 6 8 1 3 3 3 0 9 3 0 1 3 9 4
               3 3 4 2 5 1 7 2 2 4 6 8 3 1 8 0 4 3 3 5 6 2 9 1 7 5 0 1 9)
              (5 4 3)))
            ())
           (Apply Cons
            ((Apply (Int 20) ()) (Apply Vec ((Apply (Int 3) ())))))))
         (Apply (Int 1) ())))
       (Apply Cons
        ((Apply (Int 6) ())
         (Apply Cons ((Apply (Int 2) ()) (Apply Vec ((Apply (Int 5) ())))))))))
     (Apply (Int 0) ()))))))
