((ops
  (Reshape Flip Flip Reshape
   (Id
    ((8 4 6 9 1 2 5 0 8 1 9 9 8 0 8 5 4 6 4 1 8 4 0 6 1 7 6 4 3 2 9 3 8 1 8 2
      2 0 0 0 3 2 6 0 6 9 2 2 5 9 0 5 4 9 5 2 6 5 5 3 2 2 9 1 1 5 6 7 1 9 9 7
      2 2 2 9 3 5 7 1)
     (5 4 4)))
   Cons (Int 4) Vec (Int 20) (Int 1) (Int 0) Cons (Int 2) Cons (Int 5) Vec
   (Int 8)))
 (output
  (Tensor
   ((1 7 5 3 9 2 2 2 7 9 9 1 7 6 5 1 1 9 2 2 3 5 5 6 2 5 9 4 5 0 9 5 2 2 9 6
     0 6 2 3 0 0 0 2 2 8 1 8 3 9 2 3 4 6 7 1 6 0 4 8 1 4 6 4 5 8 0 8 9 9 1 8
     0 5 2 1 9 6 4 8)
    (2 5 8))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((8 4 6 9 1 2 5 0 8 1 9 9 8 0 8 5 4 6 4 1 8 4 0 6 1 7 6 4 3 2 9
               3 8 1 8 2 2 0 0 0 3 2 6 0 6 9 2 2 5 9 0 5 4 9 5 2 6 5 5 3 2 2
               9 1 1 5 6 7 1 9 9 7 2 2 2 9 3 5 7 1)
              (5 4 4)))
            ())
           (Apply Cons
            ((Apply (Int 4) ()) (Apply Vec ((Apply (Int 20) ())))))))
         (Apply (Int 1) ())))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 2) ())
       (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 8) ()))))))))))))
