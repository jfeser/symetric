open Params

module T = struct
  type t = Set.M(Int).t option [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make (T)

type concrete = bool array

let graphviz_pp params fmt m =
  if not params.hide_values then
    let pp =
      Fmt.using (function
        | None -> []
        | Some m -> Set.to_list m |> List.map ~f:(fun k -> (1, k)))
      @@ Fmt.list ~sep:(Fmt.any " ")
      @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "<sub>%d</sub>")
    in
    Fmt.pf fmt "%a" pp m

let top = Some (Set.empty (module Int))

let bot = None

let is_bottom = Option.is_none

let lift x =
  Array.to_list x
  |> List.filter_mapi ~f:(fun b v -> if not v then Some b else None)
  |> Set.of_list (module Int)
  |> Option.return

let meet x x' = Option.map2 x x' ~f:Set.inter

let is_subset s ~of_:s' =
  match (s, s') with
  | Some s, Some s' -> Set.is_subset s' ~of_:s
  | Some _, None -> false
  | None, _ -> true

let to_symb ?(prefix = sprintf "b%d") n =
  let open Symb0.Bool_vector in
  function
  | Some x ->
      Smt.(
        List.init n ~f:(fun b ->
            if Set.mem x b then return @@ Fixed false
            else fresh_decl ~prefix:(prefix b) () >>| free)
        |> all)
  | None -> failwith "bottom"

let union x x' = Option.map2 x x' ~f:Set.inter

let inter x x' = Option.map2 x x' ~f:Set.union

let sub x _ = x

let add m k v =
  Option.bind m ~f:(fun m ->
      if v then if Set.mem m k then None else Some m else Some (Set.add m k))

let contains a c =
  Option.map a ~f:(Set.for_all ~f:(fun i -> c.(i)))
  |> Option.value ~default:false

let width a = Option.map a ~f:Set.length |> Option.value ~default:0

let log_size params = function
  | None -> Float.(-infinity)
  | Some m -> Float.of_int (Array.length params.bench.output - Set.length m)

let log_overlap params s s' =
  Float.(
    log_size params (meet s s')
    - Float.min (log_size params s) (log_size params s'))
