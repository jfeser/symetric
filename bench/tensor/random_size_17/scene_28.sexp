((ops
  (Flip Reshape Reshape Flip Flip
   (Id
    ((8 8 0 3 3 3 3 1 9 5 2 3 9 9 6 4 6 5 4 6 9 9 4 5 7 4 1 6 4 1 1 2 6 7 1 2
      2 3 0 5 4 1 8 2 9 9 4 9 3 6 2 9 8 2 9 8 1 5 5 3 7 1 8 7 3 8 4 6 1 0 0 3
      6 4 4)
     (5 3 5)))
   (Int 0) (Int 2) Cons (Int 15) Vec (Int 5) Cons (Int 3) Vec (Int 25)
   (Int 1)))
 (output
  (Tensor
   ((2 9 8 2 9 9 4 9 3 6 0 3 6 4 4 8 4 6 1 0 7 1 8 7 3 4 6 5 4 6 4 1 8 2 9 2
     2 3 0 5 1 2 6 7 1 8 1 5 5 3 2 3 9 9 6 3 3 1 9 5 8 8 0 3 3 4 1 6 4 1 9 9
     4 5 7)
    (3 25))))
 (solution
  ((Apply Flip
    ((Apply Reshape
      ((Apply Reshape
        ((Apply Flip
          ((Apply Flip
            ((Apply
              (Id
               ((8 8 0 3 3 3 3 1 9 5 2 3 9 9 6 4 6 5 4 6 9 9 4 5 7 4 1 6 4 1
                 1 2 6 7 1 2 2 3 0 5 4 1 8 2 9 9 4 9 3 6 2 9 8 2 9 8 1 5 5 3
                 7 1 8 7 3 8 4 6 1 0 0 3 6 4 4)
                (5 3 5)))
              ())
             (Apply (Int 0) ())))
           (Apply (Int 2) ())))
         (Apply Cons ((Apply (Int 15) ()) (Apply Vec ((Apply (Int 5) ())))))))
       (Apply Cons ((Apply (Int 3) ()) (Apply Vec ((Apply (Int 25) ())))))))
     (Apply (Int 1) ()))))))
