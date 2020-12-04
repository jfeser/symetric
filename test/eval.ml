open Staged_synth
open Ast
open Base_quickcheck

type offset = float [@@deriving sexp_of, quickcheck]

let quickcheck_generator_offset = Generator.float_uniform_exclusive (-5.0) 5.0

let abstract_offset x =
  Option.value_exn (Abs.Offset.create ~lo:(x -. 2.0) ~hi:(x +. 2.0))

let%test_unit "cylinder abstract" =
  Test.run_exn
    ( module struct
      type t = Op.cylinder * offset * offset * Vector3.t list
      [@@deriving quickcheck, sexp_of]
    end )
    ~f:(fun (cyl, l, h, input) ->
      let al = abstract_offset l
      and ah = abstract_offset h
      and input = Array.of_list input in
      assert (
        Abs.Bool_vector.contains
          (Abs.cylinder cyl input al ah)
          (Conc.cylinder cyl input l h) ))

let%test_unit "cuboid abstract" =
  Test.run_exn
    ( module struct
      type t =
        Op.cuboid
        * offset
        * offset
        * offset
        * offset
        * offset
        * offset
        * Vector3.t list
      [@@deriving quickcheck, sexp_of]
    end )
    ~f:(fun (c, xl, xh, yl, yh, zl, zh, input) ->
      let axl = abstract_offset xl
      and axh = abstract_offset xh
      and ayl = abstract_offset yl
      and ayh = abstract_offset yh
      and azl = abstract_offset zl
      and azh = abstract_offset zh
      and input = Array.of_list input in
      assert (
        Abs.Bool_vector.contains
          (Abs.cuboid c input axl axh ayl ayh azl azh)
          (Conc.cuboid c input xl xh yl yh zl zh) ))
