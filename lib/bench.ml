module Serial = struct
  type ('o, 'v) t = { ops : 'o list; output : 'v; solution : 'o Program.t option } [@@deriving sexp]
end

type ('o, 'v) t = { filename : string option; ops : 'o list; output : 'v; solution : 'o Program.t option }
[@@deriving compare, sexp]

let load (type op v) (op_of_sexp : Sexp.t -> op) (v_of_sexp : Sexp.t -> v) fn =
  let serial = Sexp.load_sexp_conv_exn fn [%of_sexp: (op, v) Serial.t] in
  { ops = serial.ops; output = serial.output; solution = serial.solution; filename = Some fn }

let save (type op v) (sexp_of_op : op -> Sexp.t) (sexp_of_v : v -> Sexp.t) fn x =
  Sexp.save_hum fn @@ [%sexp_of: (op, v) Serial.t] { ops = x.ops; output = x.output; solution = x.solution }

let create ?filename ?solution ops output = { filename; ops; output; solution }

let output x = x.output

let ops x = x.ops

let solution x = x.solution

let solution_exn x = Option.value_exn (solution x)

let filename x = x.filename

let filename_exn x = Option.value_exn (filename x)

let param (type op value) (module Op : Sexpable.S with type t = op) (module Value : Sexpable.S with type t = value) =
  (module struct
    type nonrec t = (Op.t, Value.t) t [@@deriving sexp_of]

    let name = "bench"

    let key = Univ_map.Key.create ~name [%sexp_of: t]

    let init =
      let open Command.Param in
      anon ("bench" %: string) |> map ~f:(fun v -> Univ_map.Packed.T (key, load Op.t_of_sexp Value.t_of_sexp v))

    let to_json = Option.return @@ fun b -> `String (filename_exn b)

    let init = First init
  end : Dumb_params.Param.S
    with type t = (Op.t, Value.t) t)
