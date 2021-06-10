((ops
  (Reshape Flip Reshape Flip
   (Id
    ((1 7 2 8 9 3 3 7 5 6 4 2 3 4 3 1 9 2 8 8 7 9 1 2 2 6 4 6 3 5 4 2 5 7 9 6
      0 9 8 7 0 7 3 7 8 2 8 9 1 8 5 8 0 1 9 5 6 1 8 1)
     (4 3 5)))
   (Int 2) Cons (Int 15) Vec (Int 4) (Int 0) Cons (Int 6) Cons (Int 10) Vec
   (Int 1)))
 (output
  (Tensor
   ((8 1 6 5 0 8 5 1 8 2 9 1 0 8 1 9 8 7 3 7 8 9 0 6 5 2 4 7 4 6 9 7 7 5 3 6
     2 2 1 9 8 2 9 1 3 2 4 8 3 3 3 4 1 6 5 7 9 8 2 7)
    (6 10 1))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((1 7 2 8 9 3 3 7 5 6 4 2 3 4 3 1 9 2 8 8 7 9 1 2 2 6 4 6 3 5 4
               2 5 7 9 6 0 9 8 7 0 7 3 7 8 2 8 9 1 8 5 8 0 1 9 5 6 1 8 1)
              (4 3 5)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 15) ()) (Apply Vec ((Apply (Int 4) ())))))))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 6) ())
       (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 1) ()))))))))))))
