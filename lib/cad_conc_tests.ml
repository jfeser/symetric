open Cad_conc

let params = dummy_params ~xlen:8 ~ylen:8

let%expect_test "" =
  eval params
    (Circle { id = 0; center = { x = 3.0; y = 3.0 }; radius = 2.0 })
    []
  |> pprint Fmt.stdout;
  [%expect
    {|
........
........
........
........
.████...
.████...
........
........ |}]

let%expect_test "" =
  eval params
    (Rect
       {
         id = 0;
         lo_left = { x = 1.0; y = 1.0 };
         hi_right = { x = 4.0; y = 4.0 };
       })
    []
  |> pprint Fmt.stdout;
  [%expect
    {|
........
........
........
........
.███....
.███....
.███....
........ |}]

let%expect_test "" =
  eval params
    (Replicate { id = 0; count = 3; v = { x = 1.0; y = 1.0 } })
    [
      eval params
        (Rect
           {
             id = 1;
             lo_left = { x = 1.0; y = 1.0 };
             hi_right = { x = 4.0; y = 4.0 };
           })
        [];
    ]
  |> pprint Fmt.stdout;
  [%expect
    {|
........
........
...███..
..████..
.█████..
.████...
.███....
........ |}]

let%expect_test "" =
  eval params
    (Replicate { id = 0; count = 3; v = { x = 1.0; y = 1.0 } })
    [
      eval params Union
        [
          eval params
            (Rect
               {
                 id = 1;
                 lo_left = { x = 1.0; y = 1.0 };
                 hi_right = { x = 2.0; y = 2.0 };
               })
            [];
          eval params
            (Rect
               {
                 id = 1;
                 lo_left = { x = 3.0; y = 1.0 };
                 hi_right = { x = 6.0; y = 2.0 };
               })
            [];
        ];
    ]
  |> pprint Fmt.stdout;
  [%expect
    {|
........
........
........
........
...█.███
..█.███.
.█.███..
........ |}]

let%expect_test "" =
  eval params
    (Replicate { id = 0; count = 3; v = { x = 1.0; y = 1.0 } })
    [
      eval params Union
        [
          eval params
            (Rect
               {
                 id = 1;
                 lo_left = { x = 3.0; y = 1.0 };
                 hi_right = { x = 6.0; y = 2.0 };
               })
            [];
          eval params
            (Rect
               {
                 id = 1;
                 lo_left = { x = 1.0; y = 1.0 };
                 hi_right = { x = 2.0; y = 2.0 };
               })
            [];
        ];
    ]
  |> pprint Fmt.stdout;
  [%expect
    {|
........
........
........
........
...█.███
..█.███.
.█.███..
........ |}]
