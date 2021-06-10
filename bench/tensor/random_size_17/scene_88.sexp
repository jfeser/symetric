((ops
  (Reshape Reshape Flip
   (Id
    ((0 4 7 1 5 1 2 4 0 2 4 7 3 4 7 8 1 7 1 1 4 4 7 2 7 2 0 7 6 9 0 3 5 9 6 6
      9 2 7 2 6 5 7 2 4 6 0 6 9 5 0 4 3 3 1 9 9 6 3 7)
     (3 4 5)))
   (Int 0) Cons (Int 1) Cons (Int 4) Vec (Int 15) Cons (Int 5) Cons (Int 6)
   Vec (Int 2)))
 (output
  (Tensor
   ((6 5 7 2 4 6 0 6 9 5 0 4 3 3 1 9 9 6 3 7 4 4 7 2 7 2 0 7 6 9 0 3 5 9 6 6
     9 2 7 2 0 4 7 1 5 1 2 4 0 2 4 7 3 4 7 8 1 7 1 1)
    (5 6 2))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply
          (Id
           ((0 4 7 1 5 1 2 4 0 2 4 7 3 4 7 8 1 7 1 1 4 4 7 2 7 2 0 7 6 9 0 3
             5 9 6 6 9 2 7 2 6 5 7 2 4 6 0 6 9 5 0 4 3 3 1 9 9 6 3 7)
            (3 4 5)))
          ())
         (Apply (Int 0) ())))
       (Apply Cons
        ((Apply (Int 1) ())
         (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 15) ())))))))))
     (Apply Cons
      ((Apply (Int 5) ())
       (Apply Cons ((Apply (Int 6) ()) (Apply Vec ((Apply (Int 2) ()))))))))))))
