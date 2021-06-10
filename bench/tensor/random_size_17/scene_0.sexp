((ops
  (Reshape Flip Reshape
   (Id
    ((2 0 1 1 7 6 7 2 4 1 6 6 5 6 1 7 1 6 4 8 4 7 1 8 8 3 7 8 6 3 4 6 0 8 6 2
      1 2 4 3 1 4 1 5 1 5 5 8 1 7 0 1 2 9 8 5 3 3 5 5 8 9 5 7 9 8 9 4 7 3 2 3
      7 2 6 3 7 5 3 1)
     (4 5 4)))
   Cons (Int 4) Cons (Int 10) Vec (Int 2) (Int 0) Cons (Int 5) Cons (Int 16)
   Vec (Int 1)))
 (output
  (Tensor
   ((8 9 5 7 9 8 9 4 7 3 2 3 7 2 6 3 7 5 3 1 1 4 1 5 1 5 5 8 1 7 0 1 2 9 8 5
     3 3 5 5 4 7 1 8 8 3 7 8 6 3 4 6 0 8 6 2 1 2 4 3 2 0 1 1 7 6 7 2 4 1 6 6
     5 6 1 7 1 6 4 8)
    (5 16 1))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply
          (Id
           ((2 0 1 1 7 6 7 2 4 1 6 6 5 6 1 7 1 6 4 8 4 7 1 8 8 3 7 8 6 3 4 6
             0 8 6 2 1 2 4 3 1 4 1 5 1 5 5 8 1 7 0 1 2 9 8 5 3 3 5 5 8 9 5 7
             9 8 9 4 7 3 2 3 7 2 6 3 7 5 3 1)
            (4 5 4)))
          ())
         (Apply Cons
          ((Apply (Int 4) ())
           (Apply Cons
            ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 2) ())))))))))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 5) ())
       (Apply Cons ((Apply (Int 16) ()) (Apply Vec ((Apply (Int 1) ()))))))))))))
