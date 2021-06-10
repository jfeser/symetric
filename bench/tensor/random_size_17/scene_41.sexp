((ops
  (Reshape Reshape Flip Flip Flip
   (Id
    ((3 6 4 3 4 3 0 5 1 3 9 8 3 5 1 3 4 2 8 0 7 3 0 0 0 2 3 6 4 6 0 5 9 0 3 4
      4 9 8 0 4 4 3 0 1 6 2 6 5 0 9 9 6 2 1 8 5 1 6 4)
     (3 5 4)))
   (Int 0) (Int 2) (Int 1) Cons (Int 4) Vec (Int 15) Cons (Int 10) Vec
   (Int 6)))
 (output
  (Tensor
   ((4 6 1 5 8 1 2 6 9 9 0 5 6 2 6 1 0 3 4 4 0 8 9 4 4 3 0 9 5 0 6 4 6 3 2 0
     0 0 3 7 0 8 2 4 3 1 5 3 8 9 3 1 5 0 3 4 3 4 6 3)
    (10 6))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply Flip
            ((Apply
              (Id
               ((3 6 4 3 4 3 0 5 1 3 9 8 3 5 1 3 4 2 8 0 7 3 0 0 0 2 3 6 4 6
                 0 5 9 0 3 4 4 9 8 0 4 4 3 0 1 6 2 6 5 0 9 9 6 2 1 8 5 1 6 4)
                (3 5 4)))
              ())
             (Apply (Int 0) ())))
           (Apply (Int 2) ())))
         (Apply (Int 1) ())))
       (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 15) ())))))))
     (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 6) ()))))))))))
