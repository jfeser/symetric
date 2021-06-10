((ops
  (Flip Reshape Reshape Flip
   (Id
    ((0 1 8 1 7 6 4 4 0 3 0 7 1 3 0 8 1 0 8 7 1 4 9 3 0 9 5 1 2 3 1 1 0 1 7 6
      4 1 0 2 9 3 3 7 2 4 6 8 2 6 4 2 7 0 2 9 8 3 7 7 2 1 9 9 0 3 2 9 2 4 6 8
      6 6 1 7 8 8 5 2)
     (4 4 5)))
   (Int 0) Cons (Int 10) Cons (Int 2) Vec (Int 4) Cons (Int 16) Vec (Int 5)
   (Int 1)))
 (output
  (Tensor
   ((0 9 9 1 2 4 2 9 2 3 1 6 6 8 6 2 5 8 8 7 2 7 3 3 9 6 2 8 6 4 2 0 7 2 4 7
     7 3 8 9 0 3 9 4 1 3 2 1 5 9 7 1 0 1 1 2 0 1 4 6 7 1 8 1 0 3 0 4 4 6 0 3
     1 7 0 7 8 0 1 8)
    (16 5))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((0 1 8 1 7 6 4 4 0 3 0 7 1 3 0 8 1 0 8 7 1 4 9 3 0 9 5 1 2 3 1
               1 0 1 7 6 4 1 0 2 9 3 3 7 2 4 6 8 2 6 4 2 7 0 2 9 8 3 7 7 2 1
               9 9 0 3 2 9 2 4 6 8 6 6 1 7 8 8 5 2)
              (4 4 5)))
            ())
           (Apply (Int 0) ())))
         (Apply Cons
          ((Apply (Int 10) ())
           (Apply Cons ((Apply (Int 2) ()) (Apply Vec ((Apply (Int 4) ())))))))))
       (Apply Cons ((Apply (Int 16) ()) (Apply Vec ((Apply (Int 5) ())))))))
     (Apply (Int 1) ()))))))
