open Dumb_params

let bench_param =
  (module struct
    type t = Cad_bench.t [@@deriving sexp_of]

    let name = "bench"

    let key = Univ_map.Key.create ~name [%sexp_of: t]

    let init =
      let open Command.Param in
      anon ("bench" %: string) |> map ~f:(fun v -> Univ_map.Packed.T (key, Cad_bench.load v))

    let to_json = Option.return @@ fun b -> `String (Cad_bench.filename_exn b)

    let init = First init
  end : Param.S
    with type t = Cad_bench.t)

let spec = Spec.create ~name:"cad-params" ()

let bench = Spec.add spec @@ Param.create bench_param
