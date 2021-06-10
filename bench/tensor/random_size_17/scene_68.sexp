((ops
  (Flip Reshape Reshape Flip
   (Id
    ((5 4 1 5 0 4 9 2 0 3 8 9 8 5 3 9 3 6 7 4 7 7 8 0 7 5 0 2 3 6 1 7 1 1 9 3
      5 2 1 8 2 3 9 3 8 0 0 1 8 6 2 6 6 5 7 8 2 7 1 2)
     (3 4 5)))
   (Int 2) Cons (Int 4) Cons (Int 15) Vec (Int 1) Cons (Int 3) Vec (Int 20)
   (Int 0)))
 (output
  (Tensor
   ((8 3 9 3 2 6 8 1 0 0 7 5 6 6 2 2 1 7 2 8 7 0 8 7 7 6 3 2 0 5 9 1 1 7 1 8
     1 2 5 3 0 5 1 4 5 3 0 2 9 4 3 5 8 9 8 4 7 6 3 9)
    (3 20))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((5 4 1 5 0 4 9 2 0 3 8 9 8 5 3 9 3 6 7 4 7 7 8 0 7 5 0 2 3 6 1
               7 1 1 9 3 5 2 1 8 2 3 9 3 8 0 0 1 8 6 2 6 6 5 7 8 2 7 1 2)
              (3 4 5)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons
          ((Apply (Int 4) ())
           (Apply Cons
            ((Apply (Int 15) ()) (Apply Vec ((Apply (Int 1) ())))))))))
       (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 20) ())))))))
     (Apply (Int 0) ()))))))
