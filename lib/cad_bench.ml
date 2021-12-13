type grid = { xmax : int; ymax : int } [@@deriving sexp]

module Serial = struct
  type t = { ops : Cad_op.t list; input : grid; output : int list; solution : Cad_op.t Program.t option [@sexp.option] }
  [@@deriving sexp]
end

type t = {
  filename : string option;
  ops : Cad_op.t list;
  input : grid;
  output : Cad_conc0.t;
  solution : Cad_op.t Program.t option;
}
[@@deriving sexp]

let points g =
  List.init g.xmax ~f:(fun x ->
      List.init g.ymax ~f:(fun y -> Vector2.{ x = Float.of_int x +. 0.5; y = Float.of_int y +. 0.5 }))
  |> List.concat

let pixels input conc = points input |> List.map ~f:(Cad_conc0.getp conc)

let of_serial ?filename (x : Serial.t) =
  let output =
    Cad_conc0.create ~xlen:x.input.xmax ~ylen:x.input.ymax @@ Bitarray.of_list @@ List.map ~f:(fun x -> x > 0) x.output
  in
  { ops = x.ops; input = x.input; output; solution = x.solution; filename }

let load fn = Sexp.load_sexp_conv_exn fn [%of_sexp: Serial.t] |> of_serial ~filename:fn

let save fn b =
  let output = pixels b.input b.output |> List.map ~f:(fun x -> if x then 1 else 0) in
  Sexp.save_hum fn @@ [%sexp_of: Serial.t] @@ Serial.{ ops = b.ops; input = b.input; output; solution = b.solution }

let t_of_sexp s = [%of_sexp: Serial.t] s |> of_serial
let output x = x.output
let ops x = x.ops
let solution_exn x = Option.value_exn x.solution
let filename_exn x = Option.value_exn x.filename
