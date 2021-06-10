((ops
  (Flip Reshape Flip Reshape Flip
   (Id
    ((5 5 0 2 5 9 9 6 0 2 1 8 0 5 0 2 5 0 0 5 4 1 8 0 7 0 7 8 2 3 9 4 3 1 2 9
      9 0 3 7)
     (4 5 2)))
   (Int 2) Cons (Int 8) Vec (Int 5) (Int 0) Cons (Int 4) Vec (Int 10)
   (Int 1)))
 (output
  (Tensor
   ((9 3 1 9 4 3 7 9 0 2 0 8 0 4 1 2 3 7 8 7 2 0 5 1 8 0 5 5 0 0 9 0 2 5 5 0
     2 9 6 5)
    (4 10))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Flip
        ((Apply Reshape
          ((Apply Flip
            ((Apply
              (Id
               ((5 5 0 2 5 9 9 6 0 2 1 8 0 5 0 2 5 0 0 5 4 1 8 0 7 0 7 8 2 3
                 9 4 3 1 2 9 9 0 3 7)
                (4 5 2)))
              ())
             (Apply (Int 2) ())))
           (Apply Cons ((Apply (Int 8) ()) (Apply Vec ((Apply (Int 5) ())))))))
         (Apply (Int 0) ())))
       (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 10) ())))))))
     (Apply (Int 1) ()))))))
