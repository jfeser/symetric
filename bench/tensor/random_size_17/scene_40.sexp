((ops
  (Flip Reshape Flip Reshape
   (Id
    ((5 4 9 2 9 5 8 9 9 9 8 2 7 9 3 6 8 7 9 0 8 0 2 0 7 3 8 0 8 5 3 5 6 6 5 8
      5 3 8 2)
     (5 4 2)))
   Cons (Int 4) Vec (Int 10) (Int 0) Cons (Int 1) Cons (Int 8) Vec (Int 5)
   (Int 2)))
 (output
  (Tensor
   ((5 6 6 5 3 2 8 3 5 8 7 0 2 0 8 5 8 0 8 3 3 9 7 2 8 0 9 7 8 6 9 2 9 4 5 9
     9 9 8 5)
    (1 8 5))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((5 4 9 2 9 5 8 9 9 9 8 2 7 9 3 6 8 7 9 0 8 0 2 0 7 3 8 0 8 5 3
               5 6 6 5 8 5 3 8 2)
              (5 4 2)))
            ())
           (Apply Cons
            ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 10) ())))))))
         (Apply (Int 0) ())))
       (Apply Cons
        ((Apply (Int 1) ())
         (Apply Cons ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 5) ())))))))))
     (Apply (Int 2) ()))))))
