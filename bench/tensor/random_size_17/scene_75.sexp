((ops
  (Reshape Reshape Flip Flip
   (Id
    ((9 5 0 6 1 0 8 7 1 7 9 4 7 6 4 8 1 4 8 5 7 8 8 5 6 2 1 4 8 0 3 4 9 6 8 0
      3 2 4 9 4 3 5 9 0 8 7 9 9 7 2 5 5 5 2 2 1 8 7 6)
     (3 5 4)))
   (Int 0) (Int 2) Cons (Int 10) Vec (Int 6) Cons (Int 3) Cons (Int 20) Vec
   (Int 1)))
 (output
  (Tensor
   ((9 5 3 4 9 7 8 0 5 2 7 9 2 2 5 5 6 7 8 1 5 8 8 7 4 1 2 6 4 3 0 8 0 8 6 9
     9 4 2 3 6 0 5 9 7 8 0 1 4 9 7 1 8 4 6 7 5 8 4 1)
    (3 20 1))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((9 5 0 6 1 0 8 7 1 7 9 4 7 6 4 8 1 4 8 5 7 8 8 5 6 2 1 4 8 0 3
               4 9 6 8 0 3 2 4 9 4 3 5 9 0 8 7 9 9 7 2 5 5 5 2 2 1 8 7 6)
              (3 5 4)))
            ())
           (Apply (Int 0) ())))
         (Apply (Int 2) ())))
       (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 6) ())))))))
     (Apply Cons
      ((Apply (Int 3) ())
       (Apply Cons ((Apply (Int 20) ()) (Apply Vec ((Apply (Int 1) ()))))))))))))
