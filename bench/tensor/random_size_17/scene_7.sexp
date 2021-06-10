((ops
  (Reshape Reshape Flip Flip Flip
   (Id
    ((2 3 2 7 5 3 6 9 3 6 2 5 0 8 8 7 5 5 6 8 8 5 9 8 3 5 9 9 0 5) (2 5 3)))
   (Int 0) (Int 1) (Int 2) Cons (Int 5) Vec (Int 6) Cons (Int 10) Vec
   (Int 3)))
 (output
  (Tensor
   ((5 0 9 9 5 3 8 9 5 8 8 6 5 5 7 8 8 0 5 2 6 3 9 6 3 5 7 2 3 2) (10 3))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply Flip
            ((Apply
              (Id
               ((2 3 2 7 5 3 6 9 3 6 2 5 0 8 8 7 5 5 6 8 8 5 9 8 3 5 9 9 0 5)
                (2 5 3)))
              ())
             (Apply (Int 0) ())))
           (Apply (Int 1) ())))
         (Apply (Int 2) ())))
       (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 6) ())))))))
     (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 3) ()))))))))))
