((ops
  (Reshape Reshape Flip
   (Id
    ((8 8 5 3 7 6 1 5 4 5 8 8 8 4 9 4 6 4 3 6 9 5 5 2 1 2 3 9 1 1 7 6 1 7 1 1
      7 4 4 1 0 4 1 3 9 9 9 0 6 0 8 5 5 4 6 8 7 4 3 5 8 4 6 5 1 5 1 9 5 6 5 5
      9 8 9 7 7 5 5 6 1 3 3 6 0 8 9 0 4 7 6 5 7 9 7 0 8 1 3 8)
     (5 4 5)))
   (Int 0) Cons (Int 25) Cons (Int 1) Vec (Int 4) Cons (Int 5) Cons (Int 10)
   Vec (Int 2)))
 (output
  (Tensor
   ((1 3 3 6 0 8 9 0 4 7 6 5 7 9 7 0 8 1 3 8 8 4 6 5 1 5 1 9 5 6 5 5 9 8 9 7
     7 5 5 6 0 4 1 3 9 9 9 0 6 0 8 5 5 4 6 8 7 4 3 5 9 5 5 2 1 2 3 9 1 1 7 6
     1 7 1 1 7 4 4 1 8 8 5 3 7 6 1 5 4 5 8 8 8 4 9 4 6 4 3 6)
    (5 10 2))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply
          (Id
           ((8 8 5 3 7 6 1 5 4 5 8 8 8 4 9 4 6 4 3 6 9 5 5 2 1 2 3 9 1 1 7 6
             1 7 1 1 7 4 4 1 0 4 1 3 9 9 9 0 6 0 8 5 5 4 6 8 7 4 3 5 8 4 6 5
             1 5 1 9 5 6 5 5 9 8 9 7 7 5 5 6 1 3 3 6 0 8 9 0 4 7 6 5 7 9 7 0
             8 1 3 8)
            (5 4 5)))
          ())
         (Apply (Int 0) ())))
       (Apply Cons
        ((Apply (Int 25) ())
         (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 4) ())))))))))
     (Apply Cons
      ((Apply (Int 5) ())
       (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 2) ()))))))))))))
