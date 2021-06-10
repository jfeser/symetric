((ops
  (Reshape Flip Reshape
   (Id
    ((5 5 7 4 6 8 5 1 0 7 5 5 0 7 1 8 3 6 7 8 7 4 9 6 5 2 2 7 2 4 2 8 9 3 4 7
      3 8 1 0 0 8 7 2 7 5 6 1 2 8 5 7 1 4 7 4 4 3 2 0)
     (3 5 4)))
   Cons (Int 15) Cons (Int 4) Vec (Int 1) (Int 0) Cons (Int 2) Cons (Int 5)
   Vec (Int 6)))
 (output
  (Tensor
   ((4 3 2 0 1 4 7 4 2 8 5 7 7 5 6 1 0 8 7 2 3 8 1 0 9 3 4 7 2 4 2 8 5 2 2 7
     7 4 9 6 3 6 7 8 0 7 1 8 0 7 5 5 6 8 5 1 5 5 7 4)
    (2 5 6))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply
          (Id
           ((5 5 7 4 6 8 5 1 0 7 5 5 0 7 1 8 3 6 7 8 7 4 9 6 5 2 2 7 2 4 2 8
             9 3 4 7 3 8 1 0 0 8 7 2 7 5 6 1 2 8 5 7 1 4 7 4 4 3 2 0)
            (3 5 4)))
          ())
         (Apply Cons
          ((Apply (Int 15) ())
           (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 1) ())))))))))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 2) ())
       (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 6) ()))))))))))))
