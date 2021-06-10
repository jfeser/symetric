((ops
  (Reshape Flip Reshape
   (Id
    ((8 5 9 1 4 6 0 5 9 7 5 2 1 7 9 5 1 2 4 8 8 2 6 4 9 0 8 1 3 4 8 5 4 3 5 3
      0 5 0 0 2 4 8 9 6 1 5 9 9 7 5 3 2 4 1 1 2 9 7 0)
     (3 5 4)))
   Cons (Int 6) Cons (Int 1) Vec (Int 10) (Int 2) Cons (Int 4) Cons (Int 3)
   Vec (Int 5)))
 (output
  (Tensor
   ((7 9 5 0 6 4 1 9 5 8 8 4 2 1 5 9 7 1 2 5 4 3 1 8 0 9 4 6 2 8 0 0 5 0 3 5
     3 4 5 8 7 9 9 5 1 6 9 8 4 2 0 7 9 2 1 1 4 2 3 5)
    (4 3 5))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply
          (Id
           ((8 5 9 1 4 6 0 5 9 7 5 2 1 7 9 5 1 2 4 8 8 2 6 4 9 0 8 1 3 4 8 5
             4 3 5 3 0 5 0 0 2 4 8 9 6 1 5 9 9 7 5 3 2 4 1 1 2 9 7 0)
            (3 5 4)))
          ())
         (Apply Cons
          ((Apply (Int 6) ())
           (Apply Cons
            ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 10) ())))))))))
       (Apply (Int 2) ())))
     (Apply Cons
      ((Apply (Int 4) ())
       (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 5) ()))))))))))))
