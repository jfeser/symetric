((ops
  (Flip Flip Reshape Reshape Flip
   (Id
    ((8 3 4 6 1 7 3 4 4 4 4 8 3 0 1 8 7 3 0 8 5 6 3 4 0 5 7 7 4 5 6 2 3 4 1 9
      1 7 1 4 5 0 5 9 9 6 7 1 3 0 8 6 7 6 7 0 4 6 5 7)
     (4 3 5)))
   (Int 1) Vec (Int 60) Cons (Int 3) Cons (Int 4) Vec (Int 5) (Int 2)
   (Int 0)))
 (output
  (Tensor
   ((1 4 3 2 6 7 5 6 4 0 7 6 7 6 8 0 3 1 7 6 0 4 3 6 5 8 0 3 7 8 9 9 5 0 5 4
     1 7 1 9 1 0 3 8 4 4 4 4 3 7 1 6 4 3 8 5 4 7 7 5)
    (3 4 5))))
 (solution
  ((Apply Flip
    ((Apply Flip
      ((Apply Reshape
        ((Apply Reshape
          ((Apply Flip
            ((Apply
              (Id
               ((8 3 4 6 1 7 3 4 4 4 4 8 3 0 1 8 7 3 0 8 5 6 3 4 0 5 7 7 4 5
                 6 2 3 4 1 9 1 7 1 4 5 0 5 9 9 6 7 1 3 0 8 6 7 6 7 0 4 6 5 7)
                (4 3 5)))
              ())
             (Apply (Int 1) ())))
           (Apply Vec ((Apply (Int 60) ())))))
         (Apply Cons
          ((Apply (Int 3) ())
           (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 5) ())))))))))
       (Apply (Int 2) ())))
     (Apply (Int 0) ()))))))
