((ops
  (Flip Reshape Flip Reshape
   (Id
    ((3 6 2 0 1 2 3 9 7 3 3 4 8 4 2 9 2 8 4 6 1 4 5 2 7 8 9 5 4 7 5 0 2 9 7 2
      7 9 9 9 6 3 3 2 1 5 2 6 2 2 8 2 4 2 2 1 9 9 6 2 7 2 3 3 3 5 3 1 8 2 2 9
      8 5 6 5 1 1 0 1 8 3 5 6 9 5 9 8 6 9 4 6 6 3 8 7 3 7 2 8)
     (4 5 5)))
   Cons (Int 5) Vec (Int 20) (Int 0) Cons (Int 1) Cons (Int 4) Vec (Int 25)
   (Int 2)))
 (output
  (Tensor
   ((3 3 3 2 7 8 2 7 3 7 8 3 6 6 4 9 6 8 9 5 9 6 5 3 8 2 2 6 2 5 1 2 3 3 6 1
     0 1 1 5 6 5 8 9 2 2 8 1 3 5 7 9 2 0 5 7 4 5 9 8 7 2 5 4 1 2 6 9 9 1 2 2
     4 2 8 6 4 8 2 9 2 4 8 4 3 3 7 9 3 2 1 0 2 6 3 9 9 9 7 2)
    (1 4 25))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((3 6 2 0 1 2 3 9 7 3 3 4 8 4 2 9 2 8 4 6 1 4 5 2 7 8 9 5 4 7 5
               0 2 9 7 2 7 9 9 9 6 3 3 2 1 5 2 6 2 2 8 2 4 2 2 1 9 9 6 2 7 2
               3 3 3 5 3 1 8 2 2 9 8 5 6 5 1 1 0 1 8 3 5 6 9 5 9 8 6 9 4 6 6
               3 8 7 3 7 2 8)
              (4 5 5)))
            ())
           (Apply Cons
            ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 20) ())))))))
         (Apply (Int 0) ())))
       (Apply Cons
        ((Apply (Int 1) ())
         (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 25) ())))))))))
     (Apply (Int 2) ()))))))
