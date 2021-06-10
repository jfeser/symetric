((ops
  (Reshape Flip Reshape Flip
   (Id
    ((1 8 1 0 1 7 0 0 4 7 9 4 1 2 2 0 4 4 4 0 1 8 3 1 3 5 8 1 1 3 0 2 9 7 9 7
      6 8 6 3 3 7 8 9 4 5 0 1 8 1 4 7 5 1 0 1 0 4 0 5)
     (5 4 3)))
   (Int 0) Cons (Int 4) Vec (Int 15) (Int 1) Cons (Int 3) Cons (Int 10) Vec
   (Int 2)))
 (output
  (Tensor
   ((6 8 6 5 0 4 0 1 0 1 5 7 4 1 8 3 1 1 8 5 3 1 0 5 4 9 8 7 3 3 1 0 4 4 4 0
     2 2 1 7 9 7 9 2 0 4 9 7 4 0 0 7 1 0 1 8 1 1 3 8)
    (3 10 2))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((1 8 1 0 1 7 0 0 4 7 9 4 1 2 2 0 4 4 4 0 1 8 3 1 3 5 8 1 1 3 0
               2 9 7 9 7 6 8 6 3 3 7 8 9 4 5 0 1 8 1 4 7 5 1 0 1 0 4 0 5)
              (5 4 3)))
            ())
           (Apply (Int 0) ())))
         (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 15) ())))))))
       (Apply (Int 1) ())))
     (Apply Cons
      ((Apply (Int 3) ())
       (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 2) ()))))))))))))
