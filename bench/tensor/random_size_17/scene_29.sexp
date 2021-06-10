((ops
  (Reshape Reshape Flip Flip
   (Id
    ((3 1 8 5 8 5 1 9 5 5 1 6 3 8 4 1 4 5 0 7 6 2 4 3 8 2 3 5 2 5 3 3 0 9 7 0
      9 3 5 2 2 4 6 3 3 8 7 9)
     (4 3 4)))
   (Int 2) (Int 0) Cons (Int 8) Vec (Int 6) Cons (Int 12) Cons (Int 4) Vec
   (Int 1)))
 (output
  (Tensor
   ((2 5 3 9 3 6 4 2 9 7 8 3 5 3 2 8 3 3 5 2 0 7 9 0 1 4 8 3 7 0 5 4 3 4 2 6
     5 8 1 3 9 1 5 8 6 1 5 5)
    (12 4 1))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((3 1 8 5 8 5 1 9 5 5 1 6 3 8 4 1 4 5 0 7 6 2 4 3 8 2 3 5 2 5 3
               3 0 9 7 0 9 3 5 2 2 4 6 3 3 8 7 9)
              (4 3 4)))
            ())
           (Apply (Int 2) ())))
         (Apply (Int 0) ())))
       (Apply Cons ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 6) ())))))))
     (Apply Cons
      ((Apply (Int 12) ())
       (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 1) ()))))))))))))
