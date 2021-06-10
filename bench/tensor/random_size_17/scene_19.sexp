((ops
  (Reshape Flip Reshape Flip
   (Id
    ((1 4 2 3 7 5 1 0 7 0 6 5 7 1 9 5 9 3 2 0 9 0 5 4 1 1 7 7 7 9 1 5 7 8 2 9
      0 1 1 2 2 6 9 3 0 1 8 6 3 4 0 2 4 0 1 2 0 3 4 4)
     (3 4 5)))
   (Int 2) Cons (Int 10) Vec (Int 6) (Int 0) Cons (Int 1) Cons (Int 3) Vec
   (Int 20)))
 (output
  (Tensor
   ((0 4 4 3 0 2 8 1 1 0 4 2 9 6 2 4 3 6 1 1 0 9 0 3 2 8 7 5 1 2 9 9 7 7 7 1
     9 5 1 4 5 0 7 5 6 0 2 3 7 0 1 5 9 1 7 3 2 4 1 0)
    (1 3 20))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((1 4 2 3 7 5 1 0 7 0 6 5 7 1 9 5 9 3 2 0 9 0 5 4 1 1 7 7 7 9 1
               5 7 8 2 9 0 1 1 2 2 6 9 3 0 1 8 6 3 4 0 2 4 0 1 2 0 3 4 4)
              (3 4 5)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 6) ())))))))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 1) ())
       (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 20) ()))))))))))))
