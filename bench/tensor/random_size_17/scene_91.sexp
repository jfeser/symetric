((ops
  (Flip Reshape Flip Reshape
   (Id
    ((4 5 0 1 1 8 5 5 9 9 5 7 9 4 4 2 8 1 1 7 2 8 2 5 0 5 4 6 0 5 9 6 8 8 9 8
      9 7 8 7 0 4 7 4 3 0 4 9 8 5 5 2 1 8 9 8 9 0 1 9)
     (4 3 5)))
   Cons (Int 4) Cons (Int 5) Vec (Int 3) (Int 0) Cons (Int 10) Vec (Int 6)
   (Int 1)))
 (output
  (Tensor
   ((5 5 8 9 4 0 9 8 9 8 1 2 8 6 9 9 1 0 8 7 9 8 9 8 3 4 7 4 0 7 2 7 1 1 8 2
     4 5 0 5 2 8 0 5 4 5 0 6 9 5 5 8 1 1 4 4 9 7 5 9)
    (10 6))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((4 5 0 1 1 8 5 5 9 9 5 7 9 4 4 2 8 1 1 7 2 8 2 5 0 5 4 6 0 5 9
               6 8 8 9 8 9 7 8 7 0 4 7 4 3 0 4 9 8 5 5 2 1 8 9 8 9 0 1 9)
              (4 3 5)))
            ())
           (Apply Cons
            ((Apply (Int 4) ())
             (Apply Cons
              ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 3) ())))))))))
         (Apply (Int 0) ())))
       (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 6) ())))))))
     (Apply (Int 1) ()))))))
