((ops
  (Reshape Flip Flip Reshape Flip
   (Id
    ((6 8 4 7 6 7 5 3 0 2 1 2 7 6 1 4 9 3 2 0 1 8 3 8 8 4 6 7 2 8 1 5 8 8 5 8
      7 1 5 9 7 5 2 4 3 2 1 8 5 1 3 6 0 5 9 8 6 0 5 5 2 5 5 5 2 0 9 3 5 6 6 6
      5 7 1 3 6 2 8 0)
     (4 4 5)))
   (Int 2) Cons (Int 8) Vec (Int 10) (Int 0) (Int 1) Cons (Int 16) Vec
   (Int 5)))
 (output
  (Tensor
   ((3 6 2 8 0 6 6 5 7 1 0 9 3 5 6 2 5 5 5 2 8 6 0 5 5 3 6 0 5 9 2 1 8 5 1 7
     5 2 4 3 8 7 1 5 9 1 5 8 8 5 4 6 7 2 8 1 8 3 8 8 4 9 3 2 0 1 2 7 6 1 7 5
     3 0 2 6 8 4 7 6)
    (16 5))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Flip
        ((Apply Reshape
          ((Apply Flip
            ((Apply
              (Id
               ((6 8 4 7 6 7 5 3 0 2 1 2 7 6 1 4 9 3 2 0 1 8 3 8 8 4 6 7 2 8
                 1 5 8 8 5 8 7 1 5 9 7 5 2 4 3 2 1 8 5 1 3 6 0 5 9 8 6 0 5 5
                 2 5 5 5 2 0 9 3 5 6 6 6 5 7 1 3 6 2 8 0)
                (4 4 5)))
              ())
             (Apply (Int 2) ())))
           (Apply Cons
            ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 10) ())))))))
         (Apply (Int 0) ())))
       (Apply (Int 1) ())))
     (Apply Cons ((Apply (Int 16) ()) (Apply Vec ((Apply (Int 5) ()))))))))))
