(** Defines the sexp serialization format for 3d cad benchmarks. *)
module Serial = struct
  type offset = { offset : float; type_ : Csg_type.Offset.t } [@@deriving sexp]

  type op =
    | Union
    | Inter
    | Sub
    | Sphere of Csg_op.sphere
    | Cylinder of Csg_op.cylinder
    | Cuboid of Csg_op.cuboid
    | Offset of offset
  [@@deriving sexp]

  type t = {
    ops : op list;
    input : (float * float * float) array;
    output : int array;
  }
  [@@deriving sexp]
end

include Csg_bench0

let get_offsets ops =
  List.filter_map ops ~f:(function Serial.Offset x -> Some x | _ -> None)
  |> List.map ~f:(fun x -> (x.type_, x.offset))
  |> Map.of_alist_multi (module Csg_type.Offset)
  |> Map.map ~f:(fun l ->
         l |> List.dedup_and_sort ~compare:[%compare: float] |> List.to_array)

let convert_op offsets = function
  | Serial.Union -> Csg_op.Union
  | Inter -> Inter
  | Sub -> Sub
  | Sphere x -> Sphere x
  | Cylinder x -> Cylinder x
  | Cuboid x -> Cuboid x
  | Offset x ->
      let type_ = x.type_ and value = x.offset in
      let arr = Map.find_exn offsets type_ in
      let idx, _ =
        Array.findi arr ~f:(fun _ v -> Float.(value = v)) |> Option.value_exn
      in
      Offset { idx; arr; type_ }

let of_serial (serial : Serial.t) =
  let offsets = get_offsets serial.ops in
  let ops = List.map serial.ops ~f:(convert_op offsets) in
  {
    offsets;
    ops;
    input = Array.map serial.input ~f:(fun (x, y, z) -> Vector3.{ x; y; z });
    output =
      Array.map serial.output ~f:(function
        | 0 -> false
        | 1 -> true
        | v -> raise_s [%message "Expected 0/1" (v : int)]);
  }

let t_of_sexp s = [%of_sexp: Serial.t] s |> of_serial

let n_bits x = Array.length x.output

let offsets x = x.offsets

let output x = Csg_conc.bool_vector x.output

let ops x = x.ops

let%expect_test "" =
  ignore @@ [%of_sexp: t]
  @@ Sexp.of_string
       {|((ops ((Sphere ((center ((x 0) (y 0) (z 0))) (radius 1)))
          (Sphere ((center ((x 1) (y 1) (z 1))) (radius 1)))))
 (input ((-0.202990 2.118532 0.175085) (-0.939177 1.751782 -0.173279)
         (-0.307483 -0.788509 1.130767) (0.243875 0.187207 -0.467487)
         (-2.144061 -0.539866 -0.067927) (-1.712926 -0.481359 -0.162707)
         (-0.031287 -0.749917 0.699739) (-0.011412 1.039217 -0.135722)
         (-0.087809 1.733994 -2.617436) (1.037135 0.245658 0.504271)
         (-1.066169 -1.890825 0.511051) (-0.225635 1.406902 0.226672)
         (0.084837 -1.801167 -1.944419) (0.555416 1.062040 0.343232)
         (0.177087 -0.366246 -0.214047) (-0.709648 0.957181 -0.772105)
         (-0.389215 -1.246959 -0.156727) (0.034737 -1.199518 1.284413)
         (-0.010821 0.941056 0.160946) (1.443161 -0.783013 1.238388)
         (1.604246 -1.223111 -1.562349) (0.803929 -0.565267 -1.776866)
         (0.716542 -0.820217 -0.322581) (-0.995195 -0.674742 1.902474)
         (-0.894081 0.248181 0.969900) (0.497147 1.646780 2.007466)
         (0.585252 1.268536 2.202711) (0.578257 0.630222 1.850289)
         (0.251564 1.619198 1.144692) (1.282904 0.871919 0.708813)
         (2.013984 1.379061 1.135246) (-0.164466 1.653091 -0.245186)
         (1.987490 -0.646452 1.833926) (1.236696 0.928177 1.199424)
         (0.080765 0.604064 0.518425) (-0.052350 3.067612 0.684292)
         (1.010255 -0.118951 1.526810) (1.814688 0.185019 1.499669)
         (1.780187 2.177584 0.403596) (1.007097 1.645472 -0.320232)
         (1.426283 1.072216 2.119093) (1.395233 0.335476 0.281608)
         (0.984135 -0.102829 0.906434) (1.711015 2.652137 1.274125)
         (0.815317 1.634464 0.177459) (0.733888 0.125581 1.180908)
         (1.603153 1.134737 1.391403) (2.103620 1.359030 1.743012)
         (1.090296 0.903545 1.862611) (1.270630 2.318417 1.619064)))
 (output (0 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 1 0)))|}
