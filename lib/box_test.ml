let%test_module _ =
  ( module struct
    include Lattice_test.Make (Box)
  end )