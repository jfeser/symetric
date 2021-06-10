((ops
  (Flip Reshape Reshape Flip
   (Id
    ((4 0 1 5 6 9 3 7 1 8 0 3 7 1 0 3 4 1 8 5 3 4 2 7 6 4 0 0 9 3 6 0 0 2 8 8
      7 6 9 5 6 6 3 6 6 7 3 6 0 3 8 0 2 9 9 5 2 3 3 4 6 0 5 0 1 9 8 2 8 0 1 1
      0 2 3)
     (3 5 5)))
   (Int 2) Cons (Int 5) Cons (Int 1) Vec (Int 15) Cons (Int 3) Vec (Int 25)
   (Int 0)))
 (output
  (Tensor
   ((9 9 2 0 8 4 3 3 2 5 1 0 5 0 6 0 8 2 8 9 3 2 0 1 1 3 9 0 0 4 8 2 0 0 6 5
     9 6 7 8 6 6 3 6 6 3 0 6 3 7 6 5 1 0 4 8 1 7 3 9 0 1 7 3 0 5 8 1 4 3 6 7
     2 4 3)
    (3 25))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((4 0 1 5 6 9 3 7 1 8 0 3 7 1 0 3 4 1 8 5 3 4 2 7 6 4 0 0 9 3 6
               0 0 2 8 8 7 6 9 5 6 6 3 6 6 7 3 6 0 3 8 0 2 9 9 5 2 3 3 4 6 0
               5 0 1 9 8 2 8 0 1 1 0 2 3)
              (3 5 5)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons
          ((Apply (Int 5) ())
           (Apply Cons
            ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 15) ())))))))))
       (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 25) ())))))))
     (Apply (Int 0) ()))))))
