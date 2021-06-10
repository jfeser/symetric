((ops
  (Reshape Flip Reshape
   (Id
    ((0 5 9 0 3 3 4 9 4 2 8 0 0 9 9 6 6 2 3 8 6 2 9 0 7 2 2 5 7 0 4 3 0 4 4 6
      1 1 4 4 9 4 9 0 0 9 2 9 2 8 9 9 1 3 8 2 0 7 2 5)
     (3 4 5)))
   Cons (Int 2) Cons (Int 5) Vec (Int 6) (Int 0) Cons (Int 4) Cons (Int 15)
   Vec (Int 1)))
 (output
  (Tensor
   ((4 3 0 4 4 6 1 1 4 4 9 4 9 0 0 9 2 9 2 8 9 9 1 3 8 2 0 7 2 5 0 5 9 0 3 3
     4 9 4 2 8 0 0 9 9 6 6 2 3 8 6 2 9 0 7 2 2 5 7 0)
    (4 15 1))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply
          (Id
           ((0 5 9 0 3 3 4 9 4 2 8 0 0 9 9 6 6 2 3 8 6 2 9 0 7 2 2 5 7 0 4 3
             0 4 4 6 1 1 4 4 9 4 9 0 0 9 2 9 2 8 9 9 1 3 8 2 0 7 2 5)
            (3 4 5)))
          ())
         (Apply Cons
          ((Apply (Int 2) ())
           (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 6) ())))))))))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 4) ())
       (Apply Cons ((Apply (Int 15) ()) (Apply Vec ((Apply (Int 1) ()))))))))))))
