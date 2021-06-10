((ops
  (Reshape Flip Reshape Flip
   (Id
    ((7 0 9 0 7 5 8 1 1 7 9 6 8 7 2 0 1 1 8 9 0 8 7 1 0 1 1 8 3 8) (3 5 2)))
   (Int 2) Cons (Int 6) Vec (Int 5) (Int 0) Cons (Int 1) Cons (Int 10) Vec
   (Int 3)))
 (output
  (Tensor
   ((0 8 1 8 3 8 0 1 7 1 2 1 1 9 8 6 9 7 8 0 7 1 8 7 1 0 7 0 9 5) (1 10 3))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((7 0 9 0 7 5 8 1 1 7 9 6 8 7 2 0 1 1 8 9 0 8 7 1 0 1 1 8 3 8)
              (3 5 2)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 6) ()) (Apply Vec ((Apply (Int 5) ())))))))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 1) ())
       (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 3) ()))))))))))))
