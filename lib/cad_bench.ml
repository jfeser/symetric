type grid = { xmax : int; ymax : int } [@@deriving sexp]

type t = { ops : Cad_op.t list; input : grid; output : int list }
[@@deriving sexp]

let points g =
  List.init g.xmax ~f:(fun x ->
      List.init g.ymax ~f:(fun y ->
          Vector2.{ x = Float.of_int x; y = Float.of_int y }))
  |> List.concat

let output b =
  let module Key = struct
    module T = struct
      type t = float * float [@@deriving compare, sexp]
    end

    include T
    include Comparator.Make (T)
  end in
  List.zip_exn (points b.input) b.output
  |> List.filter ~f:(fun (_, v) -> v > 0)
  |> List.map ~f:Tuple.T2.get1
  |> Set.of_list (module Vector2)

let ops x = x.ops
