((ops
  (Reshape Reshape Flip Flip
   (Id
    ((1 9 2 6 3 8 0 6 3 5 3 3 2 7 0 7 4 8 6 9 0 9 0 9 7 2 9 1 2 6 1 4 2 3 1 8
      6 6 0 7 6 7 3 3 0 8 9 7)
     (3 4 4)))
   (Int 0) (Int 2) Cons (Int 4) Cons (Int 1) Vec (Int 12) Cons (Int 6) Vec
   (Int 8)))
 (output
  (Tensor
   ((8 1 3 2 7 0 6 6 3 3 7 6 7 9 8 0 9 6 8 4 9 0 9 0 1 9 2 7 4 1 6 2 6 2 9 1
     6 0 8 3 3 3 5 3 7 0 7 2)
    (6 8))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((1 9 2 6 3 8 0 6 3 5 3 3 2 7 0 7 4 8 6 9 0 9 0 9 7 2 9 1 2 6 1
               4 2 3 1 8 6 6 0 7 6 7 3 3 0 8 9 7)
              (3 4 4)))
            ())
           (Apply (Int 0) ())))
         (Apply (Int 2) ())))
       (Apply Cons
        ((Apply (Int 4) ())
         (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 12) ())))))))))
     (Apply Cons ((Apply (Int 6) ()) (Apply Vec ((Apply (Int 8) ()))))))))))
