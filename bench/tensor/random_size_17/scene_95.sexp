((ops
  (Reshape Reshape Flip Flip
   (Id
    ((4 1 5 2 1 9 1 7 4 9 2 4 4 2 0 3 3 9 6 6 9 5 6 9 2 1 4 6 1 1 0 3 5 8 8 9
      4 2 2 0 2 9 3 6 3 5 9 2)
     (4 4 3)))
   (Int 0) (Int 2) Cons (Int 8) Vec (Int 6) Cons (Int 12) Cons (Int 1) Vec
   (Int 4)))
 (output
  (Tensor
   ((2 2 4 9 2 0 3 6 3 2 9 5 4 1 2 1 1 6 5 3 0 9 8 8 0 2 4 9 3 3 9 6 6 9 6 5
     5 1 4 9 1 2 4 7 1 4 2 9)
    (12 1 4))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((4 1 5 2 1 9 1 7 4 9 2 4 4 2 0 3 3 9 6 6 9 5 6 9 2 1 4 6 1 1 0
               3 5 8 8 9 4 2 2 0 2 9 3 6 3 5 9 2)
              (4 4 3)))
            ())
           (Apply (Int 0) ())))
         (Apply (Int 2) ())))
       (Apply Cons ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 6) ())))))))
     (Apply Cons
      ((Apply (Int 12) ())
       (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 4) ()))))))))))))
