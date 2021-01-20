open Staged_synth
open Ast
open Base_quickcheck

type offset = float [@@deriving sexp_of, quickcheck]

let quickcheck_generator_offset = Generator.float_uniform_exclusive (-5.0) 5.0

type offset_pair = float * float [@@deriving sexp_of, quickcheck]

let quickcheck_generator_offset_pair =
  let open Generator.Let_syntax in
  let%bind lo = [%generator: offset] in
  let%map hi = Generator.float_uniform_exclusive lo 5.0 in
  (lo, hi)

type offset_ctx = offset list [@@deriving sexp_of, quickcheck]

let quickcheck_generator_offset_ctx =
  let open Generator.Let_syntax in
  Generator.list_with_length ~length:5 [%generator: offset]
  >>| List.sort ~compare:[%compare: float]

let quickcheck_shrinker_offset_ctx = Shrinker.atomic

type offset_ctx_with_pair = offset_ctx * offset * offset
[@@deriving sexp_of, quickcheck]

let quickcheck_generator_offset_ctx_with_pair =
  let open Generator.Let_syntax in
  let%bind ctx = [%generator: offset_ctx] in
  let%bind lo = Generator.of_list @@ List.drop_last_exn ctx in
  let%map hi =
    Generator.of_list @@ List.filter ctx ~f:(fun x -> Float.(x >= lo))
  in
  (ctx, lo, hi)

type input = Vector3.t list [@@deriving sexp_of, quickcheck]

let quickcheck_generator_input =
  Generator.list_with_length ~length:100 [%generator: Vector3.t]

let abstract_offset x =
  Option.value_exn
    (Abs.Offset.create ~lo:(x -. 2.0) ~hi:(x +. 2.0) ~type_:Offset_type.dummy)

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

let abs_contains_symb abs symb =
  match abs with
  | Abs.Bool_vector.Bottom -> Smt.return false
  | Map abs ->
      let open Smt.Let_syntax in
      let%bind () =
        (* Check if the symbolic can differ from the abstract on any bit *)
        List.mapi symb ~f:(fun i v ->
            match v with
            | Symb.Bool_vector.Fixed b ->
                Map.find abs i
                |> Option.iter ~f:(fun b' -> [%test_result: bool] ~expect:b' b);
                Smt.bool false
            | Free v ->
                (* Check if the symbolic output can differ from the abstract output
                   on the ith bit. The default is false. *)
                Map.find abs i
                |> Option.map ~f:(fun b -> Smt.(not (var v = bool b)))
                |> Option.value ~default:(Smt.bool false))
        |> Smt.or_ |> Smt.assert_
      in
      (* If the symbolic can differ from the abstract, then the abstract does not
         contain the symbolic behavior *)
      Smt.(check_sat) >>| not

let%test_unit "cylinder symbolic" =
  Test.run_exn
    ~config:{ Test.default_config with test_count = 150 }
    ( module struct
      type t = Op.cylinder * offset_ctx_with_pair * input
      [@@deriving quickcheck, sexp_of]
    end )
    ~f:(fun (cyl, (ctx, l, h), input) ->
      let ctx = Offset.of_list Offset_type.dummy ctx in
      let al = abstract_offset l
      and ah = abstract_offset h
      and input = Array.of_list input in
      [%test_pred: Abs.Bool_vector.t]
        (fun abs ->
          let open Smt.Let_syntax in
          let smt =
            let%bind sl = Symb.Offset.of_abs ctx al
            and sh = Symb.Offset.of_abs ctx ah in
            let%bind symb = Symb.cylinder cyl input ctx sl sh in
            abs_contains_symb abs symb
          in
          Smt.eval smt)
        (Abs.cylinder cyl input al ah))

(* let%test_unit "cuboid symbolic" =
 *   Test.run_exn
 *     ~config:{ Test.default_config with test_count = 150 }
 *     ( module struct
 *       type t =
 *         Op.cuboid
 *         * offset_ctx_with_pair
 *         * offset_ctx_with_pair
 *         * offset_ctx_with_pair
 *         * input
 *       [@@deriving quickcheck, sexp_of]
 *     end )
 *     ~f:(fun (cub, (xctx, xl, xh), (yctx, yl, yh), (zctx, zl, zh), input) ->
 *       let xctx = Offset.of_list Offset_type.dummy xctx
 *       and yctx = Offset.of_list Offset_type.dummy yctx
 *       and zctx = Offset.of_list Offset_type.dummy zctx
 *       and axl = abstract_offset xl
 *       and axh = abstract_offset xh
 *       and ayl = abstract_offset yl
 *       and ayh = abstract_offset yh
 *       and azl = abstract_offset zl
 *       and azh = abstract_offset zh
 *       and input = Array.of_list input in
 *       [%test_pred: Abs.Bool_vector.t]
 *         (fun abs ->
 *           let open Smt.Let_syntax in
 *           let smt =
 *             let%bind sxl = Symb.Offset.of_abs xctx axl
 *             and sxh = Symb.Offset.of_abs xctx axh
 *             and syl = Symb.Offset.of_abs yctx ayl
 *             and syh = Symb.Offset.of_abs yctx ayh
 *             and szl = Symb.Offset.of_abs zctx azl
 *             and szh = Symb.Offset.of_abs zctx azh in
 *             let%bind symb = Symb.cuboid cub input ctx sl sh in
 *             abs_contains_symb abs symb
 *           in
 *           Smt.eval smt)
 *         (Abs.cylinder cyl input al ah)) *)
