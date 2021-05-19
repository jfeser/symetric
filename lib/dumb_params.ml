module type Spec_intf = sig
  type t [@@deriving sexp_of]

  val type_ : t Csv.typ

  val name : string

  val doc : string

  val from_cli : bool

  val to_csv : bool

  val default : t option

  val key : t Univ_map.Key.t
end

type t = { specs : (module Spec_intf) list; values : Univ_map.t }

let arg_type (type t) (typ : t Csv.typ) : t Command.Arg_type.t =
  let open Command.Param in
  match typ with
  | Csv.Int -> int
  | Bool -> bool
  | String -> string
  | Float -> float
  | Span -> Arg_type.map float ~f:Time.Span.of_sec
  | Opt _ -> failwith "unsupported"

let param (module S : Spec_intf) =
  let open Command.Param in
  if S.from_cli then
    let param =
      match S.default with
      | Some d ->
          flag_optional_with_default_doc S.name ~doc:S.doc ~default:d
            (arg_type S.type_) S.sexp_of_t
      | None -> flag S.name ~doc:S.doc (required @@ arg_type S.type_)
    in
    let wrapped = map param ~f:(fun v -> Univ_map.Packed.T (S.key, v)) in
    Some wrapped
  else None

let cli m =
  let compare (module S : Spec_intf) (module S' : Spec_intf) =
    [%compare: string] S.name S'.name
  in
  List.filter_map m ~f:param |> Command.Param.all
  |> Command.Param.map ~f:(fun vs ->
         { specs = List.sort ~compare m; values = Univ_map.of_alist_exn vs })

let int ~name ~doc ?(cli = true) ?(csv = true) ?default () =
  (module struct
    type t = int [@@deriving sexp_of]

    let type_ = Csv.Int

    let name = name

    let doc = doc

    let from_cli = cli

    let to_csv = csv

    let default = default

    let key = Univ_map.Key.create ~name [%sexp_of: t]
  end : Spec_intf)

let bool ~name ~doc ?(cli = true) ?(csv = true) ?default () =
  (module struct
    type t = bool [@@deriving sexp_of]

    let type_ = Csv.Bool

    let name = name

    let doc = doc

    let from_cli = cli

    let to_csv = csv

    let default = default

    let key = Univ_map.Key.create ~name [%sexp_of: t]
  end : Spec_intf)

let float ~name ~doc ?(cli = true) ?(csv = true) ?default () =
  (module struct
    type t = float [@@deriving sexp_of]

    let type_ = Csv.Float

    let name = name

    let doc = doc

    let from_cli = cli

    let to_csv = csv

    let default = default

    let key = Univ_map.Key.create ~name [%sexp_of: t]
  end : Spec_intf)

let csv m =
  List.filter m.specs ~f:(fun (module P : Spec_intf) -> P.to_csv)
  |> List.map ~f:(fun (module P : Spec_intf) ->
         Csv.Field (P.type_, Univ_map.find_exn m.values P.key))
  |> Csv.to_string

let csv_header m =
  List.filter m ~f:(fun (module P : Spec_intf) -> P.to_csv)
  |> List.map ~f:(fun (module P : Spec_intf) -> Csv.Field (Csv.String, P.name))
  |> Csv.to_string
