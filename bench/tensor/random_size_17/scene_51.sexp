((ops
  (Reshape Flip Reshape Flip
   (Id
    ((8 6 9 1 1 1 5 2 6 7 5 3 6 2 8 5 9 0 1 5 8 8 1 8 0 8 7 3 8 6 3 9 0 5 5 0
      5 4 0 2 8 4 0 0 8 0 7 5 7 6 0 9 0 1 5 6 4 6 3 0)
     (4 3 5)))
   (Int 1) Cons (Int 4) Cons (Int 5) Vec (Int 3) (Int 0) Cons (Int 10) Vec
   (Int 6)))
 (output
  (Tensor
   ((6 4 6 3 0 0 9 0 1 5 0 7 5 7 6 8 4 0 0 8 0 5 4 0 2 3 9 0 5 5 8 7 3 8 6 8
     8 1 8 0 5 9 0 1 5 5 3 6 2 8 1 5 2 6 7 8 6 9 1 1)
    (10 6))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((8 6 9 1 1 1 5 2 6 7 5 3 6 2 8 5 9 0 1 5 8 8 1 8 0 8 7 3 8 6 3
               9 0 5 5 0 5 4 0 2 8 4 0 0 8 0 7 5 7 6 0 9 0 1 5 6 4 6 3 0)
              (4 3 5)))
            ())
           (Apply (Int 1) ())))
         (Apply Cons
          ((Apply (Int 4) ())
           (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 3) ())))))))))
       (Apply (Int 0) ())))
     (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 6) ()))))))))))
