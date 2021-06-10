((ops
  (Reshape Flip Reshape
   (Id
    ((5 2 2 6 6 9 8 7 5 0 5 9 8 4 4 6 8 1 5 5 5 9 5 8 7 4 3 9 4 9 9 7 7 4 2 6
      0 2 9 7 6 3 2 4 2 3 6 6 8 3 7 4 8 5 9 5 9 2 6 8 0 2 1 7 6 8 3 0 2 9 2 6
      7 1 8 5 3 4 4 8 0 3 2 2 2 0 4 3 1 1 8 4 2 1 0 9 1 5 3 8)
     (4 5 5)))
   Cons (Int 5) Cons (Int 10) Vec (Int 2) (Int 0) Cons (Int 1) Cons (Int 25)
   Vec (Int 4)))
 (output
  (Tensor
   ((0 3 2 2 2 0 4 3 1 1 8 4 2 1 0 9 1 5 3 8 0 2 1 7 6 8 3 0 2 9 2 6 7 1 8 5
     3 4 4 8 6 3 2 4 2 3 6 6 8 3 7 4 8 5 9 5 9 2 6 8 5 9 5 8 7 4 3 9 4 9 9 7
     7 4 2 6 0 2 9 7 5 2 2 6 6 9 8 7 5 0 5 9 8 4 4 6 8 1 5 5)
    (1 25 4))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply
          (Id
           ((5 2 2 6 6 9 8 7 5 0 5 9 8 4 4 6 8 1 5 5 5 9 5 8 7 4 3 9 4 9 9 7
             7 4 2 6 0 2 9 7 6 3 2 4 2 3 6 6 8 3 7 4 8 5 9 5 9 2 6 8 0 2 1 7
             6 8 3 0 2 9 2 6 7 1 8 5 3 4 4 8 0 3 2 2 2 0 4 3 1 1 8 4 2 1 0 9
             1 5 3 8)
            (4 5 5)))
          ())
         (Apply Cons
          ((Apply (Int 5) ())
           (Apply Cons
            ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 2) ())))))))))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 1) ())
       (Apply Cons ((Apply (Int 25) ()) (Apply Vec ((Apply (Int 4) ()))))))))))))
