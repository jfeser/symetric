((ops
  (Flip Reshape Flip Reshape
   (Id
    ((8 4 4 9 2 7 2 1 5 4 1 8 4 3 5 2 2 0 7 8 6 7 2 4 3 8 5 3 5 2 8 6 3 0 6 9
      7 8 5 9 9 1 5 4 8 4 8 3 9 3 5 3 2 2 6 5 4 0 0 5 7 2 2 4 7 0 0 3 7 9 9 0
      5 0 6)
     (5 5 3)))
   Cons (Int 25) Vec (Int 3) (Int 0) Cons (Int 15) Cons (Int 1) Vec (Int 5)
   (Int 2)))
 (output
  (Tensor
   ((9 9 6 0 5 4 7 3 0 0 2 2 7 0 7 5 6 5 0 0 9 2 2 3 4 3 8 4 5 3 9 9 8 4 5 0
     5 8 7 1 3 6 8 9 6 8 3 2 5 3 7 4 2 7 5 0 2 2 6 8 1 4 5 3 4 9 5 1 2 8 4 4
     8 7 2)
    (15 1 5))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((8 4 4 9 2 7 2 1 5 4 1 8 4 3 5 2 2 0 7 8 6 7 2 4 3 8 5 3 5 2 8
               6 3 0 6 9 7 8 5 9 9 1 5 4 8 4 8 3 9 3 5 3 2 2 6 5 4 0 0 5 7 2
               2 4 7 0 0 3 7 9 9 0 5 0 6)
              (5 5 3)))
            ())
           (Apply Cons
            ((Apply (Int 25) ()) (Apply Vec ((Apply (Int 3) ())))))))
         (Apply (Int 0) ())))
       (Apply Cons
        ((Apply (Int 15) ())
         (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 5) ())))))))))
     (Apply (Int 2) ()))))))
