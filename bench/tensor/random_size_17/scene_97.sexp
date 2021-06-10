((ops
  (Reshape Flip Reshape Flip
   (Id
    ((3 7 3 4 9 0 6 3 9 6 3 2 6 1 6 8 2 3 6 9 1 4 4 9 0 2 9 7 1 3 5 6 0 9 2 2
      1 3 7 4 2 9 1 3 0)
     (3 3 5)))
   (Int 2) Cons (Int 3) Vec (Int 15) (Int 0) Cons (Int 5) Cons (Int 1) Vec
   (Int 9)))
 (output
  (Tensor
   ((2 9 0 6 5 4 7 3 1 2 0 3 1 9 2 9 6 3 2 8 0 9 4 4 1 3 1 7 9 2 9 4 3 7 3 6
     9 3 6 0 6 1 6 2 3)
    (5 1 9))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((3 7 3 4 9 0 6 3 9 6 3 2 6 1 6 8 2 3 6 9 1 4 4 9 0 2 9 7 1 3 5
               6 0 9 2 2 1 3 7 4 2 9 1 3 0)
              (3 3 5)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 15) ())))))))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 5) ())
       (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 9) ()))))))))))))
