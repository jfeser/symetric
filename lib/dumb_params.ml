module type Param_intf = sig
  type t [@@deriving sexp_of]

  val name : string

  val to_json : (t -> Yojson.Basic.t) option

  val init : (Univ_map.Packed.t Command.Param.t, unit -> t) Either.t

  val key : t Univ_map.Key.t
end

type t = { spec : (module Param_intf) list; values : Univ_map.t }

module Spec = struct
  type t = (module Param_intf) list ref

  let create () = ref []

  let add (type t) q ((module S : Param_intf with type t = t) as p) =
    q := (module S : Param_intf) :: !q;
    p

  let union qs = ref (List.concat_map qs ~f:( ! ))

  let cli m =
    let compare (module S : Param_intf) (module S' : Param_intf) = [%compare: string] S.name S'.name in
    List.map !m ~f:(fun (module S : Param_intf) ->
        match S.init with First p -> p | Second f -> Command.Param.return (Univ_map.Packed.T (S.key, f ())))
    |> Command.Param.all
    |> Command.Param.map ~f:(fun vs -> { spec = List.sort ~compare !m; values = Univ_map.of_alist_exn vs })
end

module Param = struct
  module type S = Param_intf

  type free

  type bound

  type ('a, 'b) t = (module Param_intf with type t = 'a)

  type 'a mk =
    name:string ->
    doc:string ->
    ?init:[ `Cli of 'a option | `Default of unit -> 'a ] ->
    ?json:bool ->
    ?aliases:string list ->
    unit ->
    ('a, free) t

  let make_param ~name ~doc ?default ?(aliases = []) key arg_type sexp_of_t =
    let open Command.Param in
    let param =
      match default with
      | Some d -> flag_optional_with_default_doc name ~doc ~default:d ~aliases arg_type sexp_of_t
      | None -> flag name ~aliases ~doc (required arg_type)
    in
    map param ~f:(fun v -> Univ_map.Packed.T (key, v))

  let make_init ~name ~doc ?aliases key arg_type sexp_of_t = function
    | `Cli default -> First (make_param ~name ~doc ?default ?aliases key arg_type sexp_of_t)
    | `Default f -> Second f

  module Json = struct
    let float x = `Float x

    let bool x = `Bool x

    let string x = `String x

    let int x = `Int x

    let list x = `List x
  end

  let mk_to_json json f = if json then Some f else None

  let create = Fun.id

  let const_default x = `Default (Fun.const x)

  let int ~name ~doc ?(init = `Cli None) ?(json = true) ?aliases () =
    (module struct
      type t = int [@@deriving sexp_of]

      let key = Univ_map.Key.create ~name [%sexp_of: t]

      let name = name

      let to_json = mk_to_json json Json.int

      let init = make_init ~name ~doc ?aliases key Command.Param.int sexp_of_t init
    end : Param_intf
      with type t = int)

  let bool ~name ~doc ?(init = `Cli None) ?(json = true) ?aliases () =
    (module struct
      type t = bool [@@deriving sexp_of]

      let key = Univ_map.Key.create ~name [%sexp_of: t]

      let name = name

      let to_json = mk_to_json json Json.bool

      let init = make_init ~name ~doc ?aliases key Command.Param.bool sexp_of_t init
    end : Param_intf
      with type t = bool)

  let float ~name ~doc ?(init = `Cli None) ?(json = true) ?aliases () =
    (module struct
      type t = float [@@deriving sexp_of]

      let key = Univ_map.Key.create ~name [%sexp_of: t]

      let name = name

      let to_json = mk_to_json json Json.float

      let init = make_init ~name ~doc ?aliases key Command.Param.float sexp_of_t init
    end : Param_intf
      with type t = float)

  let span ~name ~doc ?(init = `Cli None) ?(json = true) ?aliases () =
    (module struct
      type t = Time.Span.t [@@deriving sexp_of]

      let key = Univ_map.Key.create ~name [%sexp_of: t]

      let name = name

      let init = make_init ~name ~doc ?aliases key Command.(Arg_type.map Param.float ~f:Time.Span.of_ms) sexp_of_t init

      let to_json = mk_to_json json @@ fun x -> Json.float (Time.Span.to_ms x)
    end : Param_intf
      with type t = Time.Span.t)

  let string ~name ~doc ?(init = `Cli None) ?(json = true) ?aliases () =
    (module struct
      type t = string [@@deriving sexp_of]

      let key = Univ_map.Key.create ~name [%sexp_of: t]

      let name = name

      let init = make_init ~name ~doc ?aliases key Command.Param.string sexp_of_t init

      let to_json = mk_to_json json Json.string
    end : Param_intf
      with type t = string)

  let mut (type t) (module P : Param_intf with type t = t) =
    (module struct
      type t = P.t ref [@@deriving sexp_of]

      let key = Univ_map.Key.create ~name:P.name [%sexp_of: P.t ref]

      let name = P.name

      let to_json = Option.map P.to_json ~f:(fun p_to_json x -> p_to_json !x)

      let init = Either.map P.init ~first:(fun _ -> failwith "") ~second:(fun mk_default () -> ref (mk_default ()))
    end : Param_intf
      with type t = P.t ref)

  let float_ref ~name ?(json = true) () = mut (float ~doc:"" ~name ~json ~init:(const_default Float.nan) ())

  let bool_ref ~name ?(default = false) ?(json = true) () =
    mut (bool ~doc:"" ~name ~json ~init:(const_default default) ())

  let span_ref ~name ?(json = true) () =
    mut (span ~doc:"" ~name ~json ~init:(const_default @@ Time.Span.of_ms Float.nan) ())

  let float_seq ~name ?(json = true) () =
    (module struct
      type t = float Queue.t [@@deriving sexp_of]

      let key = Univ_map.Key.create ~name [%sexp_of: t]

      let name = name

      let init = Second (fun () -> Queue.create ())

      let to_json = mk_to_json json @@ fun x -> Queue.to_list x |> List.map ~f:Json.float |> Json.list
    end : Param_intf
      with type t = float Queue.t)

  let float_list ~name ?(json = true) () =
    (module struct
      type t = float list Queue.t [@@deriving sexp_of]

      let key = Univ_map.Key.create ~name [%sexp_of: t]

      let name = name

      let init = Second (fun () -> Queue.create ())

      let to_json =
        if json then
          Option.return @@ fun v ->
          Json.list @@ (Queue.to_list v |> List.map ~f:(fun l -> Json.list @@ List.map l ~f:Json.float))
        else None
    end : S
      with type t = float list Queue.t)

  let const_str ~name ?(json = true) v = string ~name ~doc:"" ~init:(const_default v) ~json ()

  let ids (type t) (module Cmp : Comparator.S with type t = t) ~name ~doc ids =
    (module struct
      type nonrec t = t list

      let sexp_of_t = List.sexp_of_t Cmp.comparator.sexp_of_t

      let ids_map = Map.of_alist_exn (module String) ids

      let key = Univ_map.Key.create ~name [%sexp_of: t]

      let name = name

      let init =
        let open Command in
        let arg_type = Arg_type.of_map ids_map in
        First Param.(flag name ~doc (listed arg_type) |> map ~f:(fun v -> Univ_map.Packed.T (key, v)))

      let to_json = None
    end : Param_intf
      with type t = t list)
end

let get (type t) m (module S : Param_intf with type t = t) = Univ_map.find_exn m.values S.key

let set (type t) m (module S : Param_intf with type t = t) v = { m with values = Univ_map.set m.values S.key v }

let json m =
  let elems =
    List.filter_map m.spec ~f:(fun (module P : Param_intf) ->
        Option.map P.to_json ~f:(fun conv -> (P.name, conv @@ Univ_map.find_exn m.values P.key)))
  in
  `Assoc elems

type pair = P : ('a, Param.bound) Param.t * 'a -> pair

let of_alist_exn l =
  let cast (type t) (module P : Param.S with type t = t) = (module P : Param.S) in
  let to_packed (P ((module P), v)) = Univ_map.Packed.T (P.key, v) in
  let spec = List.map l ~f:(fun (P (k, _)) -> cast k) in
  let values = Univ_map.of_alist_exn (List.map l ~f:to_packed) in
  { spec; values }
