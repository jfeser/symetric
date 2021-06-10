((ops
  (Reshape Flip Reshape
   (Id
    ((2 1 7 6 4 5 2 0 4 8 4 1 2 7 8 6 3 1 9 6 2 3 9 5 9 7 9 3 5 5 2 3 1 2 1 1
      7 4 9 5 2 5 8 7 8 3 9 9 6 8 9 3 9 3 3 6 7 3 3 6 3 5 3 9 8 5 4 9 4 1 0 0
      7 9 2 4 9 5 4 4)
     (4 5 4)))
   Cons (Int 2) Cons (Int 4) Vec (Int 10) (Int 0) Cons (Int 5) Cons (Int 16)
   Vec (Int 1)))
 (output
  (Tensor
   ((2 5 8 7 8 3 9 9 6 8 9 3 9 3 3 6 7 3 3 6 3 5 3 9 8 5 4 9 4 1 0 0 7 9 2 4
     9 5 4 4 2 1 7 6 4 5 2 0 4 8 4 1 2 7 8 6 3 1 9 6 2 3 9 5 9 7 9 3 5 5 2 3
     1 2 1 1 7 4 9 5)
    (5 16 1))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply
          (Id
           ((2 1 7 6 4 5 2 0 4 8 4 1 2 7 8 6 3 1 9 6 2 3 9 5 9 7 9 3 5 5 2 3
             1 2 1 1 7 4 9 5 2 5 8 7 8 3 9 9 6 8 9 3 9 3 3 6 7 3 3 6 3 5 3 9
             8 5 4 9 4 1 0 0 7 9 2 4 9 5 4 4)
            (4 5 4)))
          ())
         (Apply Cons
          ((Apply (Int 2) ())
           (Apply Cons
            ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 10) ())))))))))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 5) ())
       (Apply Cons ((Apply (Int 16) ()) (Apply Vec ((Apply (Int 1) ()))))))))))))
