((ops
  (Reshape Reshape Flip
   (Id
    ((6 1 3 5 8 0 7 6 6 0 0 6 5 2 0 9 0 0 7 1 6 3 7 1 3 2 9 9 5 2 4 2 7 0 1 6
      5 9 1 7 7 5 2 7 8 0 5 1)
     (4 3 4)))
   (Int 0) Cons (Int 6) Cons (Int 2) Vec (Int 4) Cons (Int 3) Cons (Int 16)
   Vec (Int 1)))
 (output
  (Tensor
   ((5 9 1 7 7 5 2 7 8 0 5 1 3 2 9 9 5 2 4 2 7 0 1 6 5 2 0 9 0 0 7 1 6 3 7 1
     6 1 3 5 8 0 7 6 6 0 0 6)
    (3 16 1))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply
          (Id
           ((6 1 3 5 8 0 7 6 6 0 0 6 5 2 0 9 0 0 7 1 6 3 7 1 3 2 9 9 5 2 4 2
             7 0 1 6 5 9 1 7 7 5 2 7 8 0 5 1)
            (4 3 4)))
          ())
         (Apply (Int 0) ())))
       (Apply Cons
        ((Apply (Int 6) ())
         (Apply Cons ((Apply (Int 2) ()) (Apply Vec ((Apply (Int 4) ())))))))))
     (Apply Cons
      ((Apply (Int 3) ())
       (Apply Cons ((Apply (Int 16) ()) (Apply Vec ((Apply (Int 1) ()))))))))))))
