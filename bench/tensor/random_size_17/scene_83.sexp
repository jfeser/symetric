((ops
  (Reshape Reshape Flip Flip
   (Id
    ((3 3 9 2 1 0 4 8 5 1 8 0 6 5 3 7 2 5 9 8 2 8 8 7 2 7 0 7 9 7 0 3 4 1 7 1
      7 7 3 1 8 3 4 1 1 0 8 0 9 7 7 2 0 9 0 0 9 5 9 6)
     (4 5 3)))
   (Int 2) (Int 0) Cons (Int 10) Vec (Int 6) Cons (Int 4) Cons (Int 1) Vec
   (Int 15)))
 (output
  (Tensor
   ((0 8 0 7 7 9 9 0 2 9 0 0 6 9 5 4 3 0 1 7 1 3 7 7 3 8 1 1 1 4 5 2 7 2 8 9
     7 8 8 0 7 2 7 9 7 9 3 3 0 1 2 5 8 4 0 8 1 3 5 6)
    (4 1 15))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((3 3 9 2 1 0 4 8 5 1 8 0 6 5 3 7 2 5 9 8 2 8 8 7 2 7 0 7 9 7 0
               3 4 1 7 1 7 7 3 1 8 3 4 1 1 0 8 0 9 7 7 2 0 9 0 0 9 5 9 6)
              (4 5 3)))
            ())
           (Apply (Int 2) ())))
         (Apply (Int 0) ())))
       (Apply Cons ((Apply (Int 10) ()) (Apply Vec ((Apply (Int 6) ())))))))
     (Apply Cons
      ((Apply (Int 4) ())
       (Apply Cons ((Apply (Int 1) ()) (Apply Vec ((Apply (Int 15) ()))))))))))))
