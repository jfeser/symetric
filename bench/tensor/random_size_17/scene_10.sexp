((ops
  (Reshape Reshape Flip
   (Id
    ((3 9 7 6 6 8 1 5 8 7 2 2 3 1 4 5 0 0 2 3 4 8 4 0 5 5 5 4 5 3 5 2 3 1 7 2
      7 5 0 6 7 4 8 3 1 3 3 8 3 7 4 9 4 3 0 7 9 5 6 1 6 8 1 3 7 8 2 4 8 9 9 0
      8 0 4 1 4 5 2 0 9 0 6 9 6 7 2 2 3 2 4 0 2 0 5 7 9 6 3 1)
     (4 5 5)))
   (Int 0) Cons (Int 2) Cons (Int 5) Vec (Int 10) Cons (Int 25) Cons 
   (Int 1) Vec (Int 4)))
 (output
  (Tensor
   ((1 4 5 2 0 9 0 6 9 6 7 2 2 3 2 4 0 2 0 5 7 9 6 3 1 4 9 4 3 0 7 9 5 6 1 6
     8 1 3 7 8 2 4 8 9 9 0 8 0 4 5 5 4 5 3 5 2 3 1 7 2 7 5 0 6 7 4 8 3 1 3 3
     8 3 7 3 9 7 6 6 8 1 5 8 7 2 2 3 1 4 5 0 0 2 3 4 8 4 0 5)
    (25 1 4))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply
          (Id
           ((3 9 7 6 6 8 1 5 8 7 2 2 3 1 4 5 0 0 2 3 4 8 4 0 5 5 5 4 5 3 5 2
             3 1 7 2 7 5 0 6 7 4 8 3 1 3 3 8 3 7 4 9 4 3 0 7 9 5 6 1 6 8 1 3
             7 8 2 4 8 9 9 0 8 0 4 1 4 5 2 0 9 0 6 9 6 7 2 2 3 2 4 0 2 0 5 7
             9 6 3 1)
            (4 5 5)))
          ())
         (Apply (Int 0) ())))
       (Apply Cons
        ((Apply (Int 2) ())
         (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 10) ())))))))))
     (Apply Cons
      ((Apply (Int 25) ())
       (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 4) ()))))))))))))
