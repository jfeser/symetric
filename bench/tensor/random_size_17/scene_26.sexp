((ops
  (Reshape Flip Reshape Flip
   (Id
    ((5 8 7 4 0 6 2 8 2 1 2 8 7 2 9 2 4 5 0 7 6 3 3 6 8 8 6 1 2 3 5 0 0 6 9 4
      2 9 2 5 2 5 0 4 3 3 2 7 0 9 4 5 7 2 1 8 5 8 1 3 1 2 7 0 1 4 8 0 4 7 6 4
      5 5 8 3 5 9 0 7)
     (5 4 4)))
   (Int 2) Cons (Int 10) Vec (Int 8) (Int 0) Cons (Int 1) Cons (Int 16) Vec
   (Int 5)))
 (output
  (Tensor
   ((3 8 5 5 7 0 9 5 0 8 4 1 4 6 7 4 3 1 8 5 0 7 2 1 5 4 9 0 8 1 2 7 4 0 5 2
     7 2 3 3 4 9 6 0 5 2 9 2 1 6 8 8 0 5 3 2 7 0 5 4 6 3 3 6 8 2 1 2 2 9 2 7
     4 7 8 5 8 2 6 0)
    (1 16 5))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id
             ((5 8 7 4 0 6 2 8 2 1 2 8 7 2 9 2 4 5 0 7 6 3 3 6 8 8 6 1 2 3 5
               0 0 6 9 4 2 9 2 5 2 5 0 4 3 3 2 7 0 9 4 5 7 2 1 8 5 8 1 3 1 2
               7 0 1 4 8 0 4 7 6 4 5 5 8 3 5 9 0 7)
              (5 4 4)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 8) ())))))))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 1) ())
       (Apply Cons ((Apply (Int 16) ()) (Apply Vec ((Apply (Int 5) ()))))))))))))
