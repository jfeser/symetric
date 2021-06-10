((ops
  (Reshape Flip Flip Reshape Flip
   (Id
    ((3 5 5 5 1 2 6 8 8 6 9 8 7 2 3 4 7 1 3 1 0 8 0 0 1 1 8 6 9 8 6 1 2 5 2 5
      1 1 0 8 8 1 6 1 0 1 8 9 6 2 7 0 5 6 8 1 2 4 1 0 6 6 6 0 0 2 9 6 1 9 5 9
      4 7 9)
     (5 3 5)))
   (Int 2) Cons (Int 3) Vec (Int 25) (Int 1) (Int 0) Cons (Int 15) Vec
   (Int 5)))
 (output
  (Tensor
   ((5 9 4 7 9 2 9 6 1 9 6 6 6 0 0 1 2 4 1 0 7 0 5 6 8 1 8 9 6 2 8 1 6 1 0 5
     1 1 0 8 6 1 2 5 2 1 8 6 9 8 0 8 0 0 1 4 7 1 3 1 9 8 7 2 3 2 6 8 8 6 3 5
     5 5 1)
    (15 5))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Flip
        ((Apply Reshape
          ((Apply Flip
            ((Apply
              (Id
               ((3 5 5 5 1 2 6 8 8 6 9 8 7 2 3 4 7 1 3 1 0 8 0 0 1 1 8 6 9 8
                 6 1 2 5 2 5 1 1 0 8 8 1 6 1 0 1 8 9 6 2 7 0 5 6 8 1 2 4 1 0
                 6 6 6 0 0 2 9 6 1 9 5 9 4 7 9)
                (5 3 5)))
              ())
             (Apply (Int 2) ())))
           (Apply Cons
            ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 25) ())))))))
         (Apply (Int 1) ())))
       (Apply (Int 0) ())))
     (Apply Cons ((Apply (Int 15) ()) (Apply Vec ((Apply (Int 5) ()))))))))))
