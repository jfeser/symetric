((ops
  (Reshape Flip Reshape Flip Flip
   (Id
    ((6 4 3 1 3 7 3 2 5 0 4 4 6 3 1 5 4 5 6 7 7 1 6 6 3 6 1 0 6 0 3 4 0 9 1 5
      1 5 1 5 8 8 4 5 9 6 1 1 2 1 0 8 2 5 9 2 4 6 4 3 1 8 4 1 1 5 4 8 6 0 3 6
      1 6 7)
     (5 5 3)))
   (Int 2) (Int 0) Cons (Int 25) Vec (Int 3) (Int 1) Cons (Int 15) Vec
   (Int 5)))
 (output
  (Tensor
   ((1 8 4 1 1 5 4 8 6 0 3 6 1 6 7 6 1 1 2 1 0 8 2 5 9 2 4 6 4 3 3 4 0 9 1 5
     1 5 1 5 8 8 4 5 9 5 4 5 6 7 7 1 6 6 3 6 1 0 6 0 6 4 3 1 3 7 3 2 5 0 4 4
     6 3 1)
    (15 5))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply Flip
            ((Apply
              (Id
               ((6 4 3 1 3 7 3 2 5 0 4 4 6 3 1 5 4 5 6 7 7 1 6 6 3 6 1 0 6 0
                 3 4 0 9 1 5 1 5 1 5 8 8 4 5 9 6 1 1 2 1 0 8 2 5 9 2 4 6 4 3
                 1 8 4 1 1 5 4 8 6 0 3 6 1 6 7)
                (5 5 3)))
              ())
             (Apply (Int 2) ())))
           (Apply (Int 0) ())))
         (Apply Cons ((Apply (Int 25) ()) (Apply Vec ((Apply (Int 3) ())))))))
       (Apply (Int 1) ())))
     (Apply Cons ((Apply (Int 15) ()) (Apply Vec ((Apply (Int 5) ()))))))))))
