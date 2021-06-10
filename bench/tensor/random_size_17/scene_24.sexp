((ops
  (Flip Reshape Reshape
   (Id
    ((7 6 9 0 7 8 4 2 2 6 8 3 6 3 4 8 4 7 5 5 2 5 4 7 2 1 9 8 9 6 5 6 0 2 1 3
      9 6 5 2 7 8 9 6 9 3 8 5 7 8 0 6 1 0 8 1 4 2 5 0 2 9 0 3 6 7 1 4 5 4 2 8
      9 0 9 0 4 6 9 6)
     (4 5 4)))
   Cons (Int 5) Cons (Int 16) Vec (Int 1) Cons (Int 10) Cons (Int 4) Vec
   (Int 2) (Int 0)))
 (output
  (Tensor
   ((9 0 9 0 4 6 9 6 6 7 1 4 5 4 2 8 4 2 5 0 2 9 0 3 7 8 0 6 1 0 8 1 7 8 9 6
     9 3 8 5 0 2 1 3 9 6 5 2 2 1 9 8 9 6 5 6 4 7 5 5 2 5 4 7 2 6 8 3 6 3 4 8
     7 6 9 0 7 8 4 2)
    (10 4 2))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply
          (Id
           ((7 6 9 0 7 8 4 2 2 6 8 3 6 3 4 8 4 7 5 5 2 5 4 7 2 1 9 8 9 6 5 6
             0 2 1 3 9 6 5 2 7 8 9 6 9 3 8 5 7 8 0 6 1 0 8 1 4 2 5 0 2 9 0 3
             6 7 1 4 5 4 2 8 9 0 9 0 4 6 9 6)
            (4 5 4)))
          ())
         (Apply Cons
          ((Apply (Int 5) ())
           (Apply Cons
            ((Apply (Int 16) ()) (Apply Vec ((Apply (Int 1) ())))))))))
       (Apply Cons
        ((Apply (Int 10) ())
         (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 2) ())))))))))
     (Apply (Int 0) ()))))))
