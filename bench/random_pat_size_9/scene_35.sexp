((ops
  ((Circle ((id 0) (center ((x 0) (y 0))) (radius 3)))
   (Circle ((id 0) (center ((x 0) (y 10))) (radius 3)))
   (Circle ((id 0) (center ((x 0) (y 20))) (radius 3)))
   (Circle ((id 0) (center ((x 10) (y 0))) (radius 3)))
   (Circle ((id 0) (center ((x 10) (y 10))) (radius 3)))
   (Circle ((id 0) (center ((x 10) (y 20))) (radius 3)))
   (Circle ((id 0) (center ((x 20) (y 0))) (radius 3)))
   (Circle ((id 0) (center ((x 20) (y 10))) (radius 3)))
   (Circle ((id 0) (center ((x 20) (y 20))) (radius 3)))
   (Rect ((id 0) (lo_left ((x -1.5) (y -1.5))) (hi_right ((x 1.5) (y 1.5)))))
   (Rect ((id 0) (lo_left ((x -1.5) (y 8.5))) (hi_right ((x 1.5) (y 11.5)))))
   (Rect
    ((id 0) (lo_left ((x -1.5) (y 18.5))) (hi_right ((x 1.5) (y 21.5)))))
   (Rect ((id 0) (lo_left ((x 8.5) (y -1.5))) (hi_right ((x 11.5) (y 1.5)))))
   (Rect ((id 0) (lo_left ((x 8.5) (y 8.5))) (hi_right ((x 11.5) (y 11.5)))))
   (Rect
    ((id 0) (lo_left ((x 8.5) (y 18.5))) (hi_right ((x 11.5) (y 21.5)))))
   (Rect
    ((id 0) (lo_left ((x 18.5) (y -1.5))) (hi_right ((x 21.5) (y 1.5)))))
   (Rect
    ((id 0) (lo_left ((x 18.5) (y 8.5))) (hi_right ((x 21.5) (y 11.5)))))
   (Rect
    ((id 0) (lo_left ((x 18.5) (y 18.5))) (hi_right ((x 21.5) (y 21.5)))))
   (Replicate ((id 0) (count 4) (v ((x 5) (y 5)))))
   (Replicate ((id 0) (count 4) (v ((x -5) (y 5)))))
   (Replicate ((id 0) (count 4) (v ((x 5) (y -5)))))
   (Replicate ((id 0) (count 4) (v ((x -5) (y -5))))) Union Inter))
 (input ((xmax 30) (ymax 30)))
 (output
  (1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0
   0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1
   1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0
   0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 1 1 0
   0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1
   1 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0
   0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 1 1 0 0
   0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 1 1 0 0 0 0 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0
   0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1
   0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1
   1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0
   0 0 0 1 1 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 1 0 0 0 0 0 0
   1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0
   0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1
   0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0
   1 1 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
   1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0
   1 1 1 1 0 0 0 0 0 0 0 0))
 (solution
  (Apply (Replicate ((id 0) (count 4) (v ((x 5) (y 5)))))
   ((Apply (Replicate ((id 0) (count 4) (v ((x 5) (y -5)))))
     ((Apply Union
       ((Apply
         (Rect
          ((id 0) (lo_left ((x 8.5) (y 8.5))) (hi_right ((x 11.5) (y 11.5)))))
         ())
        (Apply (Replicate ((id 0) (count 4) (v ((x 5) (y 5)))))
         ((Apply (Replicate ((id 0) (count 4) (v ((x -5) (y -5)))))
           ((Apply Union
             ((Apply (Circle ((id 0) (center ((x 0) (y 10))) (radius 3))) ())
              (Apply (Replicate ((id 0) (count 4) (v ((x 5) (y -5)))))
               ((Apply
                 (Rect
                  ((id 0) (lo_left ((x -1.5) (y 8.5)))
                   (hi_right ((x 1.5) (y 11.5)))))
                 ())))))))))))))))))
