open Staged_synth
open Ast
open Base_quickcheck

type offset_pair = float * float [@@deriving sexp_of, quickcheck]

let quickcheck_generator_offset =
  let open Generator in
  let open Let_syntax in
  let%bind lo = Generator.float_uniform_exclusive (-5.0) 5.0 in
  let%map hi = Generator.float_uniform_exclusive lo 5.0 in
  (lo, hi)

type input = Vector3.t list [@@deriving sexp_of, quickcheck]

let quickcheck_generator_input =
  Generator.list_with_length ~length:100 [%generator: Vector3.t]

let abstract_offset x =
  Option.value_exn (Abs.Offset.create ~lo:(x -. 2.0) ~hi:(x +. 2.0))

let%test_unit "cylinder abstract" =
  Test.run_exn
    ( module struct
      type t = Op.cylinder * offset_pair * input
      [@@deriving quickcheck, sexp_of]
    end )
    ~f:(fun (cyl, (l, h), input) ->
      let al = abstract_offset l
      and ah = abstract_offset h
      and input = Array.of_list input in
      [%test_pred: Abs.Bool_vector.t * Conc.Bool_vector.t]
        (fun (abs, conc) -> Abs.Bool_vector.contains abs conc)
        (Abs.cylinder cyl input al ah, Conc.cylinder cyl input l h))

let%test_unit "cuboid abstract" =
  Test.run_exn
    ( module struct
      type t = Op.cuboid * offset_pair * offset_pair * offset_pair * input
      [@@deriving quickcheck, sexp_of]
    end )
    ~f:(fun (c, (xl, xh), (yl, yh), (zl, zh), input) ->
      let axl = abstract_offset xl
      and axh = abstract_offset xh
      and ayl = abstract_offset yl
      and ayh = abstract_offset yh
      and azl = abstract_offset zl
      and azh = abstract_offset zh
      and input = Array.of_list input in
      [%test_pred: Abs.Bool_vector.t * Conc.Bool_vector.t]
        (fun (abs, conc) -> Abs.Bool_vector.contains abs conc)
        ( Abs.cuboid c input axl axh ayl ayh azl azh,
          Conc.cuboid c input xl xh yl yh zl zh ))
