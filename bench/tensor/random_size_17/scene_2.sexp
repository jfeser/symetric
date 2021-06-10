((ops
  (Reshape Flip Reshape
   (Id
    ((4 2 3 6 7 5 3 3 7 7 0 3 1 2 3 0 3 1 9 4 5 4 7 5 1 9 8 0 0 3 1 4 7 9 5
      4)
     (3 3 4)))
   Cons (Int 3) Cons (Int 2) Vec (Int 6) (Int 0) Cons (Int 1) Cons (Int 9)
   Vec (Int 4)))
 (output
  (Tensor
   ((1 9 8 0 0 3 1 4 7 9 5 4 1 2 3 0 3 1 9 4 5 4 7 5 4 2 3 6 7 5 3 3 7 7 0 3)
    (1 9 4))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply
          (Id
           ((4 2 3 6 7 5 3 3 7 7 0 3 1 2 3 0 3 1 9 4 5 4 7 5 1 9 8 0 0 3 1 4
             7 9 5 4)
            (3 3 4)))
          ())
         (Apply Cons
          ((Apply (Int 3) ())
           (Apply Cons ((Apply (Int 2) ()) (Apply Vec ((Apply (Int 6) ())))))))))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 1) ())
       (Apply Cons ((Apply (Int 9) ()) (Apply Vec ((Apply (Int 4) ()))))))))))))
