((ops
  (Reshape Flip Reshape Flip Flip
   (Id
    ((9 6 6 8 8 5 6 7 4 7 4 3 9 6 5 3 6 9 8 3 0 3 0 1 1 4 5 7 1 2 6 4 7 0 9 7
      4 7 5 0 1 2 6 6 2 1 8 1)
     (4 3 4)))
   (Int 2) (Int 1) Cons (Int 4) Vec (Int 12) (Int 0) Cons (Int 6) Vec
   (Int 8)))
 (output
  (Tensor
   ((1 8 1 2 6 6 2 1 0 5 7 4 7 9 0 7 4 6 2 1 7 5 4 1 1 0 3 0 3 8 9 6 3 5 6 9
     3 4 7 4 7 6 5 8 8 6 6 9)
    (6 8))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply Flip
            ((Apply
              (Id
               ((9 6 6 8 8 5 6 7 4 7 4 3 9 6 5 3 6 9 8 3 0 3 0 1 1 4 5 7 1 2
                 6 4 7 0 9 7 4 7 5 0 1 2 6 6 2 1 8 1)
                (4 3 4)))
              ())
             (Apply (Int 2) ())))
           (Apply (Int 1) ())))
         (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 12) ())))))))
       (Apply (Int 0) ())))
     (Apply Cons ((Apply (Int 6) ()) (Apply Vec ((Apply (Int 8) ()))))))))))
