((ops
  (Reshape Reshape Flip Flip
   (Id
    ((0 5 7 9 9 5 7 3 7 2 0 4 1 2 3 4 8 9 5 1 8 8 3 2 7 4 2 9 8 2 6 9 3 8 7 5
      2 2 7 8)
     (2 5 4)))
   (Int 2) (Int 0) Cons (Int 4) Cons (Int 1) Vec (Int 10) Cons (Int 5) Vec
   (Int 8)))
 (output
  (Tensor
   ((2 3 8 8 9 2 4 7 9 6 2 8 5 7 8 3 8 7 2 2 9 7 5 0 3 7 5 9 4 0 2 7 4 3 2 1
     1 5 9 8)
    (5 8))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((0 5 7 9 9 5 7 3 7 2 0 4 1 2 3 4 8 9 5 1 8 8 3 2 7 4 2 9 8 2 6
               9 3 8 7 5 2 2 7 8)
              (2 5 4)))
            ())
           (Apply (Int 2) ())))
         (Apply (Int 0) ())))
       (Apply Cons
        ((Apply (Int 4) ())
         (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 10) ())))))))))
     (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 8) ()))))))))))
