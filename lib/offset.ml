type t = {
  idx : int;
  arr : float array; [@compare.ignore]
  type_ : Offset_type.t;
}
[@@deriving compare, hash, sexp]

type ctx = float array Map.M(Offset_type).t

let of_bench b =
  b.Bench.ops
  |> List.filter_map ~f:(function
       | Ast0.Op.Offset o -> Some (o.Bench.type_, o.offset)
       | _ -> None)
  |> Map.of_alist_multi (module Offset_type)
  |> Map.map ~f:(fun offsets ->
         let offsets = Array.of_list offsets in
         Array.sort offsets ~compare:[%compare: float];
         offsets)

let of_type ctx t =
  match Map.find ctx t with
  | Some offsets ->
      Sequence.range 0 (Array.length offsets) ~stop:`exclusive
      |> Sequence.map ~f:(fun idx -> { idx; arr = offsets; type_ = t })
  | None -> Sequence.empty

let offset x = x.arr.(x.idx)

let prev x =
  let idx' = x.idx - 1 in
  if idx' >= 0 then Some { x with idx = idx' } else None

let next x =
  let idx' = x.idx + 1 in
  if idx' < Array.length x.arr then Some { x with idx = idx' } else None

let type_ x = x.type_
