((ops
  (Reshape Reshape Flip Flip
   (Id
    ((7 3 2 8 5 8 1 3 2 0 8 8 9 9 1 3 9 6 4 6 1 7 6 8 9 5 1 2 6 3 2 0 6 1 9 3
      8 5 5 2)
     (4 2 5)))
   (Int 0) (Int 2) Cons (Int 5) Vec (Int 8) Cons (Int 10) Cons (Int 1) Vec
   (Int 4)))
 (output
  (Tensor
   ((9 1 6 0 2 2 5 5 8 3 9 8 6 7 1 3 6 2 1 5 1 9 9 8 8 6 4 6 9 3 5 8 2 3 7 0
     2 3 1 8)
    (10 1 4))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((7 3 2 8 5 8 1 3 2 0 8 8 9 9 1 3 9 6 4 6 1 7 6 8 9 5 1 2 6 3 2
               0 6 1 9 3 8 5 5 2)
              (4 2 5)))
            ())
           (Apply (Int 0) ())))
         (Apply (Int 2) ())))
       (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 8) ())))))))
     (Apply Cons
      ((Apply (Int 10) ())
       (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 4) ()))))))))))))
