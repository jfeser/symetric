((ops
  (Flip Reshape Reshape Flip Flip
   (Id
    ((1 4 3 9 6 9 8 0 6 1 2 3 3 4 1 4 5 0 8 4 6 4 6 0 0 6 5 9 3 4 6 4 6 2 1 5
      7 3 3 5 5 0 5 6 6 6 7 4 9 2 4 7 3 3 4 5 1 4 9 3)
     (5 4 3)))
   (Int 2) (Int 0) Cons (Int 20) Vec (Int 3) Cons (Int 5) Vec (Int 12)
   (Int 1)))
 (output
  (Tensor
   ((4 9 3 4 5 1 7 3 3 9 2 4 6 7 4 5 6 6 5 5 0 7 3 3 2 1 5 6 4 6 9 3 4 0 6 5
     4 6 0 8 4 6 4 5 0 3 4 1 1 2 3 8 0 6 9 6 9 1 4 3)
    (5 12))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply Flip
            ((Apply
              (Id
               ((1 4 3 9 6 9 8 0 6 1 2 3 3 4 1 4 5 0 8 4 6 4 6 0 0 6 5 9 3 4
                 6 4 6 2 1 5 7 3 3 5 5 0 5 6 6 6 7 4 9 2 4 7 3 3 4 5 1 4 9 3)
                (5 4 3)))
              ())
             (Apply (Int 2) ())))
           (Apply (Int 0) ())))
         (Apply Cons ((Apply (Int 20) ()) (Apply Vec ((Apply (Int 3) ())))))))
       (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 12) ())))))))
     (Apply (Int 1) ()))))))
