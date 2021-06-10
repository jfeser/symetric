((ops
  (Flip Flip Reshape Reshape
   (Id
    ((1 7 9 7 5 4 7 3 5 6 8 8 1 8 6 1 0 9 9 6 1 2 3 7 1 2 8 0 3 2 7 9 0 0 3 8
      4 3 6 0 0 2 0 7 5 9 6 6 2 6 0 0 2 4 8 3 2 7 6 3)
     (5 4 3)))
   Cons (Int 10) Cons (Int 2) Vec (Int 3) Cons (Int 4) Vec (Int 15) (Int 1)
   (Int 0)))
 (output
  (Tensor
   ((3 6 7 2 3 8 4 2 0 0 6 2 6 6 9 5 7 0 2 0 0 6 3 4 8 3 0 0 9 7 2 3 0 8 2 1
     7 3 2 1 6 9 9 0 1 6 8 1 8 8 6 5 3 7 4 5 7 9 7 1)
    (4 15))))
 (solution
  ((Apply Flip
    ((Apply Flip
      ((Apply Reshape
        ((Apply Reshape
          ((Apply
            (Id
             ((1 7 9 7 5 4 7 3 5 6 8 8 1 8 6 1 0 9 9 6 1 2 3 7 1 2 8 0 3 2 7
               9 0 0 3 8 4 3 6 0 0 2 0 7 5 9 6 6 2 6 0 0 2 4 8 3 2 7 6 3)
              (5 4 3)))
            ())
           (Apply Cons
            ((Apply (Int 10) ())
             (Apply Cons
              ((Apply (Int 2) ()) (Apply Vec ((Apply (Int 3) ())))))))))
         (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 15) ())))))))
       (Apply (Int 1) ())))
     (Apply (Int 0) ()))))))
