((ops
  (Reshape Flip Flip Reshape Flip
   (Id
    ((5 4 0 4 1 5 8 6 4 5 7 3 6 5 6 6 4 6 7 8 1 5 9 4 6 5 2 6 8 9 1 4 3 9 2
      6)
     (4 3 3)))
   (Int 2) Cons (Int 4) Vec (Int 9) (Int 1) (Int 0) Cons (Int 12) Vec
   (Int 3)))
 (output
  (Tensor
   ((9 2 6 1 4 3 6 8 9 6 5 2 5 9 4 7 8 1 6 4 6 6 5 6 5 7 3 8 6 4 4 1 5 5 4 0)
    (12 3))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Flip
        ((Apply Reshape
          ((Apply Flip
            ((Apply
              (Id
               ((5 4 0 4 1 5 8 6 4 5 7 3 6 5 6 6 4 6 7 8 1 5 9 4 6 5 2 6 8 9
                 1 4 3 9 2 6)
                (4 3 3)))
              ())
             (Apply (Int 2) ())))
           (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 9) ())))))))
         (Apply (Int 1) ())))
       (Apply (Int 0) ())))
     (Apply Cons ((Apply (Int 12) ()) (Apply Vec ((Apply (Int 3) ()))))))))))
