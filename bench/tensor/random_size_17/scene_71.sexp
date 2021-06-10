((ops
  (Reshape Reshape Flip Flip Flip
   (Id
    ((4 0 4 2 7 5 1 9 3 3 5 2 7 2 6 9 6 5 7 7 5 9 8 7 7 7 9 0 9 3 1 6 3 6 9 3
      1 5 0 9 0 0 9 6 8 2 6 0 8 0 6 6 4 8 4 8 6 4 6 1)
     (3 4 5)))
   (Int 2) (Int 0) (Int 1) Vec (Int 60) Cons (Int 5) Cons (Int 4) Vec
   (Int 3)))
 (output
  (Tensor
   ((1 6 4 6 8 4 8 4 6 6 0 8 0 6 2 8 6 9 0 0 9 0 5 1 3 9 6 3 6 1 3 9 0 9 7 7
     7 8 9 5 7 7 5 6 9 6 2 7 2 5 3 3 9 1 5 7 2 4 0 4)
    (5 4 3))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply Flip
            ((Apply
              (Id
               ((4 0 4 2 7 5 1 9 3 3 5 2 7 2 6 9 6 5 7 7 5 9 8 7 7 7 9 0 9 3
                 1 6 3 6 9 3 1 5 0 9 0 0 9 6 8 2 6 0 8 0 6 6 4 8 4 8 6 4 6 1)
                (3 4 5)))
              ())
             (Apply (Int 2) ())))
           (Apply (Int 0) ())))
         (Apply (Int 1) ())))
       (Apply Vec ((Apply (Int 60) ())))))
     (Apply Cons
      ((Apply (Int 5) ())
       (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 3) ()))))))))))))
