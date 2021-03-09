type grid = { xmax : int; ymax : int } [@@deriving sexp]

module Serial = struct
  type t = {
    ops : Cad_op.t list;
    input : grid;
    output : int list;
    solution : Cad_op.t Program.t option; [@sexp.option]
  }
  [@@deriving sexp]
end

type t = {
  ops : Cad_op.t list;
  input : grid;
  output : bool Map.M(Vector2).t;
  solution : Cad_op.t Program.t option;
}

let points g =
  List.init g.xmax ~f:(fun x ->
      List.init g.ymax ~f:(fun y ->
          Vector2.{ x = Float.of_int x +. 0.5; y = Float.of_int y +. 0.5 }))
  |> List.concat

let of_serial (x : Serial.t) =
  let output =
    List.map2_exn (points x.input) x.output ~f:(fun k v -> (k, v > 0))
    |> Map.of_alist_exn (module Vector2)
  in
  { ops = x.ops; input = x.input; output; solution = x.solution }

let t_of_sexp s = [%of_sexp: Serial.t] s |> of_serial

let output x = x.output

let ops x = x.ops
