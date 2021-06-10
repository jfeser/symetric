((ops
  (Flip Flip Reshape Reshape Flip
   (Id
    ((2 5 4 8 8 9 5 2 8 3 1 4 5 7 4 5 5 5 3 9 7 6 4 2 8 1 8 9 0 0 0 1 5 6 7 6
      1 4 4 2 0 4 9 1 9 7 4 0 5 3 2 2 5 7 6 7 4 5 3 5)
     (4 5 3)))
   (Int 2) Cons (Int 12) Vec (Int 5) Cons (Int 3) Vec (Int 20) (Int 1)
   (Int 0)))
 (output
  (Tensor
   ((5 3 5 6 7 4 2 5 7 5 3 2 7 4 0 9 1 9 2 0 4 1 4 4 6 7 6 0 1 5 9 0 0 8 1 8
     6 4 2 3 9 7 5 5 5 5 7 4 3 1 4 5 2 8 8 8 9 2 5 4)
    (3 20))))
 (solution
  ((Apply Flip
    ((Apply Flip
      ((Apply Reshape
        ((Apply Reshape
          ((Apply Flip
            ((Apply
              (Id
               ((2 5 4 8 8 9 5 2 8 3 1 4 5 7 4 5 5 5 3 9 7 6 4 2 8 1 8 9 0 0
                 0 1 5 6 7 6 1 4 4 2 0 4 9 1 9 7 4 0 5 3 2 2 5 7 6 7 4 5 3 5)
                (4 5 3)))
              ())
             (Apply (Int 2) ())))
           (Apply Cons
            ((Apply (Int 12) ()) (Apply Vec ((Apply (Int 5) ())))))))
         (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 20) ())))))))
       (Apply (Int 1) ())))
     (Apply (Int 0) ()))))))
