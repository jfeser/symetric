open Params

module T = struct
  type t = Map of bool Map.M(Int).t | Bottom [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

type concrete = bool array

let top = Map (Map.empty (module Int))

let bot = Bottom

let is_bottom = function Bottom -> true | _ -> false

let log_size params = function
  | Bottom -> Float.(-infinity)
  | Map m -> Float.of_int (Array.length params.bench.output - Map.length m)

let pp : t Fmt.t =
  Fmt.using (function
    | Map m -> Map.to_alist m |> List.map ~f:(fun (k, v) -> (Bool.to_int v, k))
    | Bottom -> [])
  @@ Fmt.list ~sep:(Fmt.any " ")
  @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "_%d")

let graphviz_pp params fmt m =
  if not params.hide_values then
    let pp =
      Fmt.using (function
        | Bottom -> []
        | Map m ->
            Map.to_alist m |> List.map ~f:(fun (k, v) -> (Bool.to_int v, k)))
      @@ Fmt.list ~sep:(Fmt.any " ")
      @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "<sub>%d</sub>")
    in
    Fmt.pf fmt "%a" pp m

let meet x x' =
  match (x, x') with
  | Map m, Map m' ->
      let ks = Map.key_set m and ks' = Map.key_set m' in
      Set.inter ks ks' |> Set.to_list
      |> List.map ~f:(fun k ->
             let v = Map.find_exn m k and v' = Map.find_exn m' k in
             if Bool.(v = v') then Some (k, v) else None)
      |> Option.all
      |> Option.map ~f:(fun kv -> Map (Map.of_alist_exn (module Int) kv))
      |> Option.value ~default:Bottom
  | Bottom, _ | _, Bottom -> Bottom

let%test_unit "meet" =
  [%test_result: t]
    ~expect:(Map (Map.of_alist_exn (module Int) [ (0, true); (1, false) ]))
    (meet
       (Map (Map.of_alist_exn (module Int) [ (0, true); (1, false); (2, true) ]))
       (Map (Map.of_alist_exn (module Int) [ (0, true); (1, false); (3, true) ])))

let log_overlap params s s' =
  Float.(
    log_size params (meet s s')
    - Float.min (log_size params s) (log_size params s'))

let is_subset s ~of_:s' =
  match (s, s') with
  | Bottom, _ -> true
  | Map _, Bottom -> false
  | Map s, Map s' ->
      if Map.length s < Map.length s' then false
      else
        Map.fold2 s s' ~init:true ~f:(fun ~key:_ ~data acc ->
            acc
            &&
            match data with
            | `Left _ -> true
            | `Right _ -> false
            | `Both (x, x') -> Bool.(x = x'))

let lift s =
  Map
    ( Array.mapi s ~f:(fun i x -> (i, x))
    |> Array.to_list
    |> Map.of_alist_exn (module Int) )

let to_symb ?(prefix = sprintf "b%d") n =
  let open Symb0.Bool_vector in
  function
  | Map m ->
      Smt.(
        List.init n ~f:(fun b ->
            match Map.find m b with
            | Some v -> return @@ Fixed v
            | None -> fresh_decl ~prefix:(prefix b) () >>| fun v -> Free v)
        |> all)
  | Bottom -> failwith "bottom"

let union x x' =
  match (x, x') with
  | Bottom, _ | _, Bottom -> Bottom
  | Map x, Map x' ->
      Map
        (Map.merge x x' ~f:(fun ~key:_ -> function
           | `Both (x, x') -> Some (x || x')
           | `Left true | `Right true -> Some true
           | `Left false | `Right false -> None))

let inter x x' =
  match (x, x') with
  | Bottom, _ | _, Bottom -> Bottom
  | Map x, Map x' ->
      Map
        (Map.merge x x' ~f:(fun ~key:_ -> function
           | `Both (x, x') -> Some (x && x')
           | `Left false | `Right false -> Some false
           | `Left true | `Right true -> None))

let sub x x' =
  match (x, x') with
  | Bottom, _ | _, Bottom -> Bottom
  | Map x, Map x' ->
      Map
        (Map.merge x x' ~f:(fun ~key:_ -> function
           | `Both (x, x') -> Some (x && not x')
           | `Left false | `Right true -> Some false
           | `Left true | `Right false -> None))

let mem v i = match v with Bottom -> false | Map v -> Map.mem v i

let set m k v =
  match m with Map m -> Map (Map.set m ~key:k ~data:v) | Bottom -> Bottom

let add m k v =
  match m with
  | Map m -> (
      match Map.find m k with
      | Some v' -> if Bool.(v = v') then Map m else Bottom
      | None -> set (Map m) k v )
  | Bottom -> Bottom

let contains a c =
  match a with
  | Map a -> Map.for_alli a ~f:(fun ~key:i ~data:v -> Bool.(c.(i) = v))
  | Bottom -> false

let width a = match a with Map m -> Map.length m | Bottom -> 0

let of_list_exn l = Map (Map.of_alist_exn (module Int) l)
