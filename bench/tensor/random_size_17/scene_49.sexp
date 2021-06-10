((ops
  (Reshape Flip Flip Reshape
   (Id
    ((2 0 8 4 8 3 1 6 0 7 4 0 4 2 9 1 8 7 2 5 0 9 5 6 2 1 8 3 7 5 8 3 5 1 4 7
      6 8 5 7 3 3 6 2 1 3 9 2 6 2 0 1 3 3 7 2 2 8 1 9)
     (4 3 5)))
   Cons (Int 6) Vec (Int 10) (Int 1) (Int 0) Cons (Int 4) Cons (Int 5) Vec
   (Int 3)))
 (output
  (Tensor
   ((9 1 8 2 2 7 3 3 1 0 2 6 2 9 3 1 2 6 3 3 7 5 8 6 7 4 1 5 3 8 5 7 3 8 1 2
     6 5 9 0 5 2 7 8 1 9 2 4 0 4 7 0 6 1 3 8 4 8 0 2)
    (4 5 3))))
 (solution
  ((Apply Reshape
    ((Apply Flip
      ((Apply Flip
        ((Apply Reshape
          ((Apply
            (Id
             ((2 0 8 4 8 3 1 6 0 7 4 0 4 2 9 1 8 7 2 5 0 9 5 6 2 1 8 3 7 5 8
               3 5 1 4 7 6 8 5 7 3 3 6 2 1 3 9 2 6 2 0 1 3 3 7 2 2 8 1 9)
              (4 3 5)))
            ())
           (Apply Cons
            ((Apply (Int 6) ()) (Apply Vec ((Apply (Int 10) ())))))))
         (Apply (Int 1) ())))
       (Apply (Int 0) ())))
     (Apply Cons
      ((Apply (Int 4) ())
       (Apply Cons ((Apply (Int 5) ()) (Apply Vec ((Apply (Int 3) ()))))))))))))
