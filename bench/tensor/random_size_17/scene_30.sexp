((ops
  (Reshape Flip Reshape Flip Flip
   (Id
    ((0 9 9 6 5 3 1 5 4 8 1 6 1 6 7 3 3 6 5 7 6 4 4 7 3 2 3 5 0 6 7 6 7 9 2 0
      6 5 2 6 5 9 7 6 1 3 6 8 2 2 7 3 8 2 3 3 6 9 9 4)
     (3 4 5)))
   (Int 2) (Int 0) Cons (Int 5) Vec (Int 12) (Int 1) Cons (Int 20) Vec
   (Int 3)))
 (output
  (Tensor
   ((2 3 3 6 8 2 2 5 9 7 6 1 4 4 7 3 3 6 9 9 4 7 3 8 6 7 6 7 9 2 2 3 5 0 6 6
     5 4 8 0 9 9 6 5 0 6 5 2 3 3 6 5 7 1 6 1 6 7 3 1)
    (20 3))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply Flip
            ((Apply
              (Id
               ((0 9 9 6 5 3 1 5 4 8 1 6 1 6 7 3 3 6 5 7 6 4 4 7 3 2 3 5 0 6
                 7 6 7 9 2 0 6 5 2 6 5 9 7 6 1 3 6 8 2 2 7 3 8 2 3 3 6 9 9 4)
                (3 4 5)))
              ())
             (Apply (Int 2) ())))
           (Apply (Int 0) ())))
         (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 12) ())))))))
       (Apply (Int 1) ())))
     (Apply Cons ((Apply (Int 20) ()) (Apply Vec ((Apply (Int 3) ()))))))))))
