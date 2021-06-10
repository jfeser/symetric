((ops
  (Reshape Flip Flip Reshape Flip
   (Id
    ((9 3 4 1 8 1 6 6 5 9 1 1 3 3 4 4 5 3 9 5 8 4 9 7 4 6 3 4 0 8) (3 2 5)))
   (Int 2) Cons (Int 6) Vec (Int 5) (Int 1) (Int 0) Cons (Int 3) Vec
   (Int 10)))
 (output
  (Tensor
   ((6 3 4 0 8 8 4 9 7 4 4 5 3 9 5 1 1 3 3 4 1 6 6 5 9 9 3 4 1 8) (3 10))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Flip
        ((Apply Reshape
          ((Apply Flip
            ((Apply
              (Id
               ((9 3 4 1 8 1 6 6 5 9 1 1 3 3 4 4 5 3 9 5 8 4 9 7 4 6 3 4 0 8)
                (3 2 5)))
              ())
             (Apply (Int 2) ())))
           (Apply Cons ((Apply (Int 6) ()) (Apply Vec ((Apply (Int 5) ())))))))
         (Apply (Int 1) ())))
       (Apply (Int 0) ())))
     (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 10) ()))))))))))
