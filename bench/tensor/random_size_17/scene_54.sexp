((ops
  (Reshape Flip Reshape Flip
   (Id
    ((8 8 8 2 6 2 5 6 8 7 5 3 3 3 1 3 4 8 5 3 4 5 3 1 0 4 6 4 1 6 7 1 8 6 0 4
      7 0 0 4 3 8 4 5 8)
     (5 3 3)))
   (Int 2) Cons (Int 3) Vec (Int 15) (Int 0) Cons (Int 1) Cons (Int 9) Vec
   (Int 5)))
 (output
  (Tensor
   ((8 1 7 4 0 6 0 0 7 8 3 4 8 5 4 8 4 3 4 3 5 1 3 5 6 4 0 6 1 4 8 8 8 2 6 2
     8 6 5 3 5 7 1 3 3)
    (1 9 5))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((8 8 8 2 6 2 5 6 8 7 5 3 3 3 1 3 4 8 5 3 4 5 3 1 0 4 6 4 1 6 7
               1 8 6 0 4 7 0 0 4 3 8 4 5 8)
              (5 3 3)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 15) ())))))))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 1) ())
       (Apply Cons ((Apply (Int 9) ()) (Apply Vec ((Apply (Int 5) ()))))))))))))
