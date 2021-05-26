((ops
  ((Circle ((id 0) (center ((x 28) (y 13))) (radius 8)))
   (Circle ((id 1) (center ((x 11) (y 0))) (radius 11)))
   (Rect ((id 2) (lo_left ((x 13) (y 24))) (hi_right ((x 18) (y 25)))))
   (Circle ((id 3) (center ((x 5) (y 12))) (radius 10)))
   (Replicate ((id 4) (count 3) (v ((x 2) (y -2)))))
   (Replicate ((id 5) (count 4) (v ((x -2) (y -2))))) Union Inter))
 (input ((xmax 30) (ymax 30)))
 (output
  (1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0
   0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 1 1 1 1 1
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0
   1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 1 1 1 1 1 1
   1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 1
   0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 1 1 1 1 1 1 1 1 0 0 1 0 1 0 1 0 0 0 0 0
   0 0 0 0 1 1 1 0 0 1 1 1 1 1 1 1 1 1 0 0 1 0 1 0 1 0 0 0 0 0 0 0 0 0 1 1 1
   0 0 1 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 0 0 0 0 0 0 1 1 1 0 0 1 1 1 1 1
   1 1 1 1 1 0 1 0 1 0 1 0 1 0 0 0 0 0 0 0 1 1 1 0 0 0 1 1 1 1 1 1 1 0 1 0 1
   0 1 0 1 0 1 0 1 0 0 0 0 0 1 1 1 0 0 0 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0
   1 0 0 0 0 0 1 1 1 0 0 0 1 1 1 1 1 0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 0 0 0 0 1
   1 0 0 0 1 1 1 1 1 1 0 0 0 0 0 1 0 1 0 1 0 1 0 1 0 0 0 0 0 1 1 0 0 0 1 1 1
   1 1 0 0 0 0 0 0 1 0 1 0 1 0 1 0 1 0 0 0 0 0 1 1 0 0 0 1 1 1 1 1 1 1 0 0 0
   0 0 0 1 0 1 0 1 0 0 0 0 0 0 0 1 1 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 1 0 1 0
   1 0 0 0 0 0 0 0 1 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
   0 1 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1
   1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1
   1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
   1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 0
   0 0 0 0 0 0 0 0 0 0 0 0))
 (solution
  (Apply Union
   ((Apply Union
     ((Apply (Replicate ((id 4) (count 3) (v ((x 2) (y -2)))))
       ((Apply Union
         ((Apply (Circle ((id 3) (center ((x 5) (y 12))) (radius 10))) ())
          (Apply (Circle ((id 1) (center ((x 11) (y 0))) (radius 11))) ())))))
      (Apply (Replicate ((id 5) (count 4) (v ((x -2) (y -2)))))
       ((Apply (Replicate ((id 4) (count 3) (v ((x 2) (y -2)))))
         ((Apply
           (Rect
            ((id 2) (lo_left ((x 13) (y 24))) (hi_right ((x 18) (y 25)))))
           ())))))))
    (Apply (Replicate ((id 5) (count 4) (v ((x -2) (y -2)))))
     ((Apply (Circle ((id 0) (center ((x 28) (y 13))) (radius 8))) ())))))))