module Test_op = struct
  module T = struct
    type t = Op of int [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  type type_ = unit

  let arity (Op x) = x

  let type_ _ = assert false

  let args_type _ = assert false

  let ret_type _ = assert false
end

open Test_op

open Flat_program.Make (Test_op)

let%expect_test "" =
  let open Program in
  let v = Apply (Op 0, []) in
  let x = Apply (Op 2, [ Apply (Op 3, [ v; v; v ]); v ]) in
  print_s [%message (of_program x : Test_op.t array)];
  [%test_result: Test_op.t Program.t] ~expect:x (to_program @@ of_program x);
  [%expect {| ("of_program x" ((Op 2) (Op 3) (Op 0) (Op 0) (Op 0) (Op 0))) |}]
