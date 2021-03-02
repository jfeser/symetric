type grid = { xmax : int; ymax : int } [@@deriving sexp]

type t = { ops : Cad_op.t list; input : grid; output : int list }
[@@deriving sexp]

let points g =
  List.init g.xmax ~f:(fun x ->
      List.init g.ymax ~f:(fun y ->
          Vector2.{ x = Float.of_int x; y = Float.of_int y }))
  |> List.concat

let output b =
  List.map2_exn (points b.input) b.output ~f:(fun k v -> (k, v > 0))
  |> Map.of_alist_exn (module Vector2)

let ops x = x.ops
