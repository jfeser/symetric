((ops
  (Reshape Reshape Flip Flip
   (Id
    ((9 1 7 3 7 6 7 8 9 7 5 9 7 3 6 1 7 3 0 9 9 2 0 1 3 1 4 6 2 1 9 0 0 5 2 0
      8 5 8 8 6 4 5 6 1 4 9 7)
     (4 3 4)))
   (Int 0) (Int 2) Cons (Int 4) Cons (Int 1) Vec (Int 12) Cons (Int 8) Vec
   (Int 6)))
 (output
  (Tensor
   ((8 8 5 8 6 5 4 6 7 9 4 1 6 4 1 3 0 9 1 2 0 2 5 0 1 6 3 7 9 0 3 7 1 0 2 9
     3 7 1 9 8 7 6 7 9 5 7 9)
    (8 6))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((9 1 7 3 7 6 7 8 9 7 5 9 7 3 6 1 7 3 0 9 9 2 0 1 3 1 4 6 2 1 9
               0 0 5 2 0 8 5 8 8 6 4 5 6 1 4 9 7)
              (4 3 4)))
            ())
           (Apply (Int 0) ())))
         (Apply (Int 2) ())))
       (Apply Cons
        ((Apply (Int 4) ())
         (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 12) ())))))))))
     (Apply Cons ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 6) ()))))))))))
