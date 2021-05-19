module type Param_intf = sig
  type t [@@deriving sexp_of]

  val name : string

  val to_csv : (t -> string) option

  val init : (Univ_map.Packed.t Command.Param.t, unit -> t) Either.t

  val key : t Univ_map.Key.t
end

type 'a param = (module Param_intf with type t = 'a)

type spec = (module Param_intf)

type 'a mk =
  name:string ->
  doc:string ->
  ?init:[ `Cli of 'a option | `Default of unit -> 'a ] ->
  ?csv:bool ->
  ?aliases:string list ->
  unit ->
  'a param

type t = { specs : spec list; values : Univ_map.t }

let to_spec (type t) (module S : Param_intf with type t = t) =
  (module S : Param_intf)

let cli m =
  let compare (module S : Param_intf) (module S' : Param_intf) =
    [%compare: string] S.name S'.name
  in
  List.map m ~f:(fun (module S : Param_intf) ->
      match S.init with
      | First p -> p
      | Second f -> Command.Param.return (Univ_map.Packed.T (S.key, f ())))
  |> Command.Param.all
  |> Command.Param.map ~f:(fun vs ->
         { specs = List.sort ~compare m; values = Univ_map.of_alist_exn vs })

let make_param ~name ~doc ?default ?(aliases = []) key arg_type sexp_of_t =
  let open Command.Param in
  let param =
    match default with
    | Some d ->
        flag_optional_with_default_doc name ~doc ~default:d ~aliases arg_type
          sexp_of_t
    | None -> flag name ~aliases ~doc (required arg_type)
  in
  map param ~f:(fun v -> Univ_map.Packed.T (key, v))

let make_init ~name ~doc ?aliases key arg_type sexp_of_t = function
  | `Cli default ->
      First (make_param ~name ~doc ?default ?aliases key arg_type sexp_of_t)
  | `Default f -> Second f

let make_to_csv csv f = if csv then Some f else None

let int ~name ~doc ?(init = `Cli None) ?(csv = true) ?aliases () =
  (module struct
    type t = int [@@deriving sexp_of]

    let key = Univ_map.Key.create ~name [%sexp_of: t]

    let name = name

    let to_csv = make_to_csv csv @@ sprintf "%d"

    let init =
      make_init ~name ~doc ?aliases key Command.Param.int sexp_of_t init
  end : Param_intf
    with type t = int)

let bool ~name ~doc ?(init = `Cli None) ?(csv = true) ?aliases () =
  (module struct
    type t = bool [@@deriving sexp_of]

    let key = Univ_map.Key.create ~name [%sexp_of: t]

    let name = name

    let to_csv = make_to_csv csv @@ fun x -> if x then "1" else "0"

    let init =
      make_init ~name ~doc ?aliases key Command.Param.bool sexp_of_t init
  end : Param_intf
    with type t = bool)

let float ~name ~doc ?(init = `Cli None) ?(csv = true) ?aliases () =
  (module struct
    type t = float [@@deriving sexp_of]

    let key = Univ_map.Key.create ~name [%sexp_of: t]

    let name = name

    let to_csv = make_to_csv csv @@ sprintf "%f"

    let init =
      make_init ~name ~doc ?aliases key Command.Param.float sexp_of_t init
  end : Param_intf
    with type t = float)

let float_ref ~name ?(csv = true) () =
  (module struct
    type t = float ref [@@deriving sexp_of]

    let key = Univ_map.Key.create ~name [%sexp_of: t]

    let name = name

    let init = Second (fun () -> ref Float.nan)

    let to_csv = make_to_csv csv @@ fun x -> sprintf "%f" !x
  end : Param_intf
    with type t = float ref)

let bool_ref ~name ?(default = false) ?(csv = true) () =
  (module struct
    type t = bool ref [@@deriving sexp_of]

    let key = Univ_map.Key.create ~name [%sexp_of: t]

    let name = name

    let init = Second (fun () -> ref default)

    let to_csv = make_to_csv csv @@ fun x -> if !x then "1" else "0"
  end : Param_intf
    with type t = bool ref)

let span_ref ~name ?(csv = true) () =
  (module struct
    type t = Time.Span.t ref [@@deriving sexp_of]

    let key = Univ_map.Key.create ~name [%sexp_of: t]

    let name = name

    let init = Second (fun () -> ref (Time.Span.of_ms Float.nan))

    let to_csv = make_to_csv csv @@ fun x -> sprintf "%f" @@ Time.Span.to_ms !x
  end : Param_intf
    with type t = Time.Span.t ref)

let const_str ~name ?(csv = true) v =
  (module struct
    type t = string [@@deriving sexp_of]

    let key = Univ_map.Key.create ~name [%sexp_of: t]

    let name = name

    let init = Second (fun () -> v)

    let to_csv = make_to_csv csv Fun.id
  end : Param_intf
    with type t = string)

let get (type t) m (module S : Param_intf with type t = t) =
  Univ_map.find_exn m.values S.key

type packed = P : 'a param * 'a -> packed

let of_alist_exn ms =
  let specs = List.map ms ~f:(fun (P (x, _)) -> to_spec x) in
  {
    specs;
    values =
      List.map ms ~f:(fun (P ((module P), v)) -> Univ_map.Packed.T (P.key, v))
      |> Univ_map.of_alist_exn;
  }

let csv m =
  List.filter_map m.specs ~f:(fun (module P : Param_intf) ->
      Option.map P.to_csv ~f:(fun f -> f @@ Univ_map.find_exn m.values P.key))
  |> String.concat ~sep:","

let csv_header m =
  List.filter_map m.specs ~f:(fun (module P : Param_intf) ->
      if Option.is_some P.to_csv then Some P.name else None)
  |> String.concat ~sep:","
