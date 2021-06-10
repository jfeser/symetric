((ops
  (Reshape Flip Flip Reshape
   (Id
    ((8 9 1 2 2 6 0 2 5 8 5 8 2 0 5 5 1 6 0 6 7 5 4 6 4 9 4 1 8 7 4 8 7 8 6 8
      2 8 5 1 0 9 9 8 0 0 9 3)
     (4 4 3)))
   Cons (Int 12) Vec (Int 4) (Int 1) (Int 0) Cons (Int 3) Cons (Int 8) Vec
   (Int 2)))
 (output
  (Tensor
   ((3 9 0 0 8 9 9 0 1 5 8 2 8 6 8 7 8 4 7 8 1 4 9 4 6 4 5 7 6 0 6 1 5 5 0 2
     8 5 8 5 2 0 6 2 2 1 9 8)
    (3 8 2))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((8 9 1 2 2 6 0 2 5 8 5 8 2 0 5 5 1 6 0 6 7 5 4 6 4 9 4 1 8 7 4
               8 7 8 6 8 2 8 5 1 0 9 9 8 0 0 9 3)
              (4 4 3)))
            ())
           (Apply Cons
            ((Apply (Int 12) ()) (Apply Vec ((Apply (Int 4) ())))))))
         (Apply (Int 1) ())))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 3) ())
       (Apply Cons ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 2) ()))))))))))))
