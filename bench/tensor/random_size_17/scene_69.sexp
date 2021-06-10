((ops
  (Flip Reshape Reshape
   (Id
    ((0 2 6 2 8 4 1 0 6 7 3 1 6 0 0 2 8 5 9 2 7 4 7 4 4 4 4 5 2 9 8 2 5 2 3 7
      2 5 0 7 8 6 0 5 1 1 6 3 1 2 7 7 6 1 6 7 1 5 6 9 2 2 4 1 8 1 3 9 4 2 1 2
      7 2 4 5 3 0 0 6)
     (4 5 4)))
   Cons (Int 16) Cons (Int 5) Vec (Int 1) Cons (Int 2) Cons (Int 4) Vec
   (Int 10) (Int 0)))
 (output
  (Tensor
   ((8 6 0 5 1 1 6 3 1 2 7 7 6 1 6 7 1 5 6 9 2 2 4 1 8 1 3 9 4 2 1 2 7 2 4 5
     3 0 0 6 0 2 6 2 8 4 1 0 6 7 3 1 6 0 0 2 8 5 9 2 7 4 7 4 4 4 4 5 2 9 8 2
     5 2 3 7 2 5 0 7)
    (2 4 10))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply
          (Id
           ((0 2 6 2 8 4 1 0 6 7 3 1 6 0 0 2 8 5 9 2 7 4 7 4 4 4 4 5 2 9 8 2
             5 2 3 7 2 5 0 7 8 6 0 5 1 1 6 3 1 2 7 7 6 1 6 7 1 5 6 9 2 2 4 1
             8 1 3 9 4 2 1 2 7 2 4 5 3 0 0 6)
            (4 5 4)))
          ())
         (Apply Cons
          ((Apply (Int 16) ())
           (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 1) ())))))))))
       (Apply Cons
        ((Apply (Int 2) ())
         (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 10) ())))))))))
     (Apply (Int 0) ()))))))
