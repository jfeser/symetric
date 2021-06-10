((ops
  (Flip Reshape Reshape Flip
   (Id
    ((0 8 7 6 3 2 3 8 7 8 5 5 0 6 0 3 6 0 5 5 1 8 2 8 1 2 2 8 6 6 0 7 2 9 4 5
      7 5 7 3 7 9 9 5 7 8 2 7 9 5 0 9 9 9 8 1 0 7 1 8 0 9 6 2 4 7 4 2 9 2 3 0
      2 0 7)
     (3 5 5)))
   (Int 0) Cons (Int 25) Vec (Int 3) Cons (Int 5) Cons (Int 1) Vec (Int 15)
   (Int 2)))
 (output
  (Tensor
   ((4 2 6 9 0 8 1 7 0 1 8 9 9 9 0 6 6 8 2 2 7 0 2 0 3 2 9 2 4 7 7 5 9 9 7 3
     7 5 7 5 4 9 2 7 0 8 7 8 3 2 3 6 7 8 0 5 9 7 2 8 1 8 2 8 1 5 5 0 6 3 0 6
     0 5 5)
    (5 1 15))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((0 8 7 6 3 2 3 8 7 8 5 5 0 6 0 3 6 0 5 5 1 8 2 8 1 2 2 8 6 6 0
               7 2 9 4 5 7 5 7 3 7 9 9 5 7 8 2 7 9 5 0 9 9 9 8 1 0 7 1 8 0 9
               6 2 4 7 4 2 9 2 3 0 2 0 7)
              (3 5 5)))
            ())
           (Apply (Int 0) ())))
         (Apply Cons ((Apply (Int 25) ()) (Apply Vec ((Apply (Int 3) ())))))))
       (Apply Cons
        ((Apply (Int 5) ())
         (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 15) ())))))))))
     (Apply (Int 2) ()))))))
