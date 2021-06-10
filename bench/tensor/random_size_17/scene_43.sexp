((ops
  (Reshape Reshape Flip Flip
   (Id
    ((1 8 7 1 5 8 3 3 7 5 2 1 5 7 2 3 2 2 6 2 1 7 2 6 4 5 6 2 2 4 3 5 1 3 7 7
      7 8 3 2 3 8 3 5 9 2 0 4 8 4 8 1 0 4 1 0 9 9 5 9 5 7 3 9 6 9 9 4 8 9 2 2
      7 3 4)
     (5 3 5)))
   (Int 2) (Int 0) Cons (Int 3) Vec (Int 25) Cons (Int 15) Cons (Int 1) Vec
   (Int 5)))
 (output
  (Tensor
   ((6 9 3 7 5 9 8 4 9 9 4 3 7 2 2 4 8 4 0 2 1 4 0 1 8 9 5 9 9 0 7 3 1 5 3 2
     3 8 7 7 9 5 3 8 3 2 6 2 2 3 4 6 2 7 1 4 2 2 6 5 5 1 7 8 1 5 7 3 3 8 2 7
     5 1 2)
    (15 1 5))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((1 8 7 1 5 8 3 3 7 5 2 1 5 7 2 3 2 2 6 2 1 7 2 6 4 5 6 2 2 4 3
               5 1 3 7 7 7 8 3 2 3 8 3 5 9 2 0 4 8 4 8 1 0 4 1 0 9 9 5 9 5 7
               3 9 6 9 9 4 8 9 2 2 7 3 4)
              (5 3 5)))
            ())
           (Apply (Int 2) ())))
         (Apply (Int 0) ())))
       (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 25) ())))))))
     (Apply Cons
      ((Apply (Int 15) ())
       (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 5) ()))))))))))))
