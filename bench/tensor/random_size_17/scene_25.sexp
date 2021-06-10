((ops
  (Flip Reshape Reshape
   (Id
    ((0 5 7 1 2 0 6 1 0 1 4 5 0 7 2 5 8 8 6 2 2 6 5 1 4 4 9 0 8 0 0 9 4 4 1 9
      8 7 6 3 2 1 5 7 4 8 7 2 0 6 8 1 6 6 6 9 2 1 0 1)
     (4 3 5)))
   Cons (Int 6) Cons (Int 2) Vec (Int 5) Cons (Int 4) Cons (Int 1) Vec
   (Int 15) (Int 0)))
 (output
  (Tensor
   ((8 7 2 0 6 8 1 6 6 6 9 2 1 0 1 0 9 4 4 1 9 8 7 6 3 2 1 5 7 4 5 8 8 6 2 2
     6 5 1 4 4 9 0 8 0 0 5 7 1 2 0 6 1 0 1 4 5 0 7 2)
    (4 1 15))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply
          (Id
           ((0 5 7 1 2 0 6 1 0 1 4 5 0 7 2 5 8 8 6 2 2 6 5 1 4 4 9 0 8 0 0 9
             4 4 1 9 8 7 6 3 2 1 5 7 4 8 7 2 0 6 8 1 6 6 6 9 2 1 0 1)
            (4 3 5)))
          ())
         (Apply Cons
          ((Apply (Int 6) ())
           (Apply Cons ((Apply (Int 2) ()) (Apply Vec ((Apply (Int 5) ())))))))))
       (Apply Cons
        ((Apply (Int 4) ())
         (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 15) ())))))))))
     (Apply (Int 0) ()))))))
