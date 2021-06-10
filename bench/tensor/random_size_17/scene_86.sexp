((ops
  (Reshape Reshape Flip Flip
   (Id
    ((8 4 3 2 8 8 5 6 9 6 0 0 3 6 5 4 1 2 6 9 1 7 8 5 3 3 5 9 6 0 3 6 1 1 4 2
      7 1 1 5 5 4 0 3 8 4 3 7 5 5 1 0 6 2 6 3 9 8 3 5)
     (3 4 5)))
   (Int 0) (Int 2) Cons (Int 3) Vec (Int 20) Cons (Int 1) Cons (Int 6) Vec
   (Int 10)))
 (output
  (Tensor
   ((8 3 0 4 5 5 5 7 3 4 6 2 6 0 1 5 3 8 9 3 3 5 8 7 1 0 6 9 5 3 4 1 1 6 3 5
     1 1 7 2 8 2 3 4 8 6 9 6 5 8 5 6 3 0 0 9 6 2 1 4)
    (1 6 10))))
 (solution
  ((Apply Reshape
    ((Apply Reshape
      ((Apply Flip
        ((Apply Flip
          ((Apply
            (Id
             ((8 4 3 2 8 8 5 6 9 6 0 0 3 6 5 4 1 2 6 9 1 7 8 5 3 3 5 9 6 0 3
               6 1 1 4 2 7 1 1 5 5 4 0 3 8 4 3 7 5 5 1 0 6 2 6 3 9 8 3 5)
              (3 4 5)))
            ())
           (Apply (Int 0) ())))
         (Apply (Int 2) ())))
       (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 20) ())))))))
     (Apply Cons
      ((Apply (Int 1) ())
       (Apply Cons ((Apply (Int 6) ()) (Apply Vec ((Apply (Int 10) ()))))))))))))
