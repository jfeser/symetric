((ops
  (Flip Reshape Reshape
   (Id
    ((1 5 3 7 6 8 4 0 1 0 5 9 9 2 5 3 1 1 4 0 6 6 3 6 6 9 9 5 6 2 3 2 1 9 5 6
      6 1 3 5 2 5 1 4 1 7 5 7 5 0 5 9 9 2 0 0 7 9 9 3)
     (3 4 5)))
   Cons (Int 1) Cons (Int 10) Vec (Int 6) Cons (Int 3) Cons (Int 5) Vec
   (Int 4) (Int 0)))
 (output
  (Tensor
   ((2 5 1 4 1 7 5 7 5 0 5 9 9 2 0 0 7 9 9 3 6 6 3 6 6 9 9 5 6 2 3 2 1 9 5 6
     6 1 3 5 1 5 3 7 6 8 4 0 1 0 5 9 9 2 5 3 1 1 4 0)
    (3 5 4))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply
          (Id
           ((1 5 3 7 6 8 4 0 1 0 5 9 9 2 5 3 1 1 4 0 6 6 3 6 6 9 9 5 6 2 3 2
             1 9 5 6 6 1 3 5 2 5 1 4 1 7 5 7 5 0 5 9 9 2 0 0 7 9 9 3)
            (3 4 5)))
          ())
         (Apply Cons
          ((Apply (Int 1) ())
           (Apply Cons
            ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 6) ())))))))))
       (Apply Cons
        ((Apply (Int 3) ())
         (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 4) ())))))))))
     (Apply (Int 0) ()))))))
