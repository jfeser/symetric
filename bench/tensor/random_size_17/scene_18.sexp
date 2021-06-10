((ops
  (Reshape Flip Reshape Flip
   (Id ((0 3 7 9 2 7 4 5 6 5 8 9 3 1 4 4 5 1 5 7 8 1 3 0) (2 3 4))) (Int 2)
   Cons (Int 4) Vec (Int 6) (Int 0) Cons (Int 1) Cons (Int 3) Vec (Int 8)))
 (output
  (Tensor ((1 5 0 3 1 8 4 4 1 3 7 5 7 2 9 8 5 6 9 7 3 0 5 4) (1 3 8))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Reshape
        ((Apply Flip
          ((Apply
            (Id ((0 3 7 9 2 7 4 5 6 5 8 9 3 1 4 4 5 1 5 7 8 1 3 0) (2 3 4)))
            ())
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 6) ())))))))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 1) ())
       (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 8) ()))))))))))))
