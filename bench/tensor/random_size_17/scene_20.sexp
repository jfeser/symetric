((ops
  (Flip Reshape Reshape Flip Flip
   (Id
    ((5 8 4 0 5 7 9 1 3 4 2 7 3 1 8 4 2 8 3 9 3 6 7 9 9 8 3 2 5 1 2 6 6 3 5 0
      1 1 2 3 2 4 1 2 0 8 5 4 8 0 5 4 3 9 0 2 8 2 3 9)
     (5 3 4)))
   (Int 1) (Int 2) Cons (Int 12) Vec (Int 5) Cons (Int 20) Vec (Int 3)
   (Int 0)))
 (output
  (Tensor
   ((5 0 8 9 3 4 8 2 0 9 3 2 2 1 1 4 2 3 0 2 1 4 5 8 3 8 9 1 5 2 6 6 2 0 5 3
     8 1 3 8 2 4 3 9 3 9 7 6 4 8 5 7 5 0 3 1 9 7 2 4)
    (20 3))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply Flip
            ((Apply
              (Id
               ((5 8 4 0 5 7 9 1 3 4 2 7 3 1 8 4 2 8 3 9 3 6 7 9 9 8 3 2 5 1
                 2 6 6 3 5 0 1 1 2 3 2 4 1 2 0 8 5 4 8 0 5 4 3 9 0 2 8 2 3 9)
                (5 3 4)))
              ())
             (Apply (Int 1) ())))
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 12) ()) (Apply Vec ((Apply (Int 5) ())))))))
       (Apply Cons ((Apply (Int 20) ()) (Apply Vec ((Apply (Int 3) ())))))))
     (Apply (Int 0) ()))))))
