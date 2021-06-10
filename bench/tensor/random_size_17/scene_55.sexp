((ops
  (Flip Reshape Reshape Flip
   (Id
    ((6 9 7 3 8 1 0 2 6 2 9 7 0 2 5 9 6 9 9 6 3 7 0 6 8 2 1 6 0 3 4 2 6 5 1 8
      9 2 1 5 9 8 0 4 6 3 8 3 0 8 5 8 3 5 9 6 4 7 9 2 2 7 5 2 9 0 1 4 2 4 4 3
      8 5 2 6 3 8 9 9)
     (5 4 4)))
   (Int 0) Cons (Int 20) Vec (Int 4) Cons (Int 5) Cons (Int 8) Vec (Int 2)
   (Int 1)))
 (output
  (Tensor
   ((9 9 3 8 2 6 8 5 4 3 2 4 1 4 9 0 5 2 2 7 9 2 4 7 9 6 3 5 5 8 0 8 8 3 6 3
     0 4 9 8 1 5 9 2 1 8 6 5 4 2 0 3 1 6 8 2 0 6 3 7 9 6 6 9 5 9 0 2 9 7 6 2
     0 2 8 1 7 3 6 9)
    (5 8 2))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((6 9 7 3 8 1 0 2 6 2 9 7 0 2 5 9 6 9 9 6 3 7 0 6 8 2 1 6 0 3 4
               2 6 5 1 8 9 2 1 5 9 8 0 4 6 3 8 3 0 8 5 8 3 5 9 6 4 7 9 2 2 7
               5 2 9 0 1 4 2 4 4 3 8 5 2 6 3 8 9 9)
              (5 4 4)))
            ())
           (Apply (Int 0) ())))
         (Apply Cons ((Apply (Int 20) ()) (Apply Vec ((Apply (Int 4) ())))))))
       (Apply Cons
        ((Apply (Int 5) ())
         (Apply Cons ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 2) ())))))))))
     (Apply (Int 1) ()))))))
