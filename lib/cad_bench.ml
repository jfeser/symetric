type grid = { xmax : int; ymax : int } [@@deriving sexp]

type t = { ops : Cad_op.t list; input : grid; output : int list }
[@@deriving sexp]

let output b =
  let module Key = struct
    module T = struct
      type t = float * float [@@deriving compare, sexp]
    end

    include T
    include Comparator.Make (T)
  end in
  let keys =
    List.init b.input.xmax ~f:(fun x ->
        List.init b.input.ymax ~f:(fun y -> (Float.of_int x, Float.of_int y)))
    |> List.concat
  in
  let map = List.zip_exn keys b.output |> Map.of_alist_exn (module Key) in
  ((fun x y -> Map.find_exn map (x, y) > 0), b.input)

let ops x = x.ops
