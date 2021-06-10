((ops
  (Reshape Reshape Flip Flip
   (Id
    ((7 9 0 4 2 8 4 4 9 0 5 2 0 0 5 4 1 5 9 3 6 2 2 5 8 1 4 6 5 5) (3 2 5)))
   (Int 2) (Int 0) Cons (Int 5) Vec (Int 6) Cons (Int 10) Cons (Int 3) Vec
   (Int 1)))
 (output
  (Tensor
   ((8 5 2 2 6 5 5 6 4 1 5 0 0 2 5 3 9 5 1 4 2 4 0 9 7 0 9 4 4 8) (10 3 1))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((7 9 0 4 2 8 4 4 9 0 5 2 0 0 5 4 1 5 9 3 6 2 2 5 8 1 4 6 5 5)
              (3 2 5)))
            ())
           (Apply (Int 2) ())))
         (Apply (Int 0) ())))
       (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 6) ())))))))
     (Apply Cons
      ((Apply (Int 10) ())
       (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 1) ()))))))))))))
