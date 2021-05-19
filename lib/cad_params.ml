module P = Dumb_params

let bench =
  (module struct
    type t = Cad_bench.t [@@deriving sexp_of]

    let name = "bench"

    let key = Univ_map.Key.create ~name [%sexp_of: t]

    let init =
      let open Command.Param in
      anon ("bench" %: string)
      |> map ~f:(fun v -> Univ_map.Packed.T (key, Cad_bench.load v))

    let to_csv = None

    let init = First init
  end : P.Param_intf
    with type t = Cad_bench.t)

let spec = [ P.to_spec bench ]
