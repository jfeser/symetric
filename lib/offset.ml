type t = {
  idx : int;
  arr : float array; [@compare.ignore]
  type_ : Csg_type.Offset.t;
}
[@@deriving compare, hash, sexp]

type ctx = float array Map.M(Csg_type.Offset).t [@@deriving sexp_of]

let of_list t l =
  List.map l ~f:(fun o -> (t, o))
  |> Map.of_alist_multi (module Csg_type.Offset)
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

let of_type_count ctx t =
  match Map.find ctx t with Some offsets -> Array.length offsets | None -> 0

let offset x = x.arr.(x.idx)

let prev x =
  let idx' = x.idx - 1 in
  if idx' >= 0 then Some { x with idx = idx' } else None

let next x =
  let idx' = x.idx + 1 in
  if idx' < Array.length x.arr then Some { x with idx = idx' } else None

let type_ x = x.type_

let lt ctx t x =
  match Map.find ctx t with
  | Some offsets ->
      let end_ =
        Array.binary_search offsets ~compare:[%compare: float]
          `First_greater_than_or_equal_to x
        |> Option.value ~default:0
      in
      Sequence.range 0 end_ ~stop:`exclusive
      |> Sequence.map ~f:(fun idx -> { idx; arr = offsets; type_ = t })
  | None -> Sequence.empty

let gt ctx t x =
  match Map.find ctx t with
  | Some offsets ->
      let start =
        Array.binary_search offsets ~compare:[%compare: float]
          `First_greater_than_or_equal_to x
        |> Option.value ~default:(Array.length offsets)
      in
      Sequence.range start (Array.length offsets) ~stop:`exclusive
      |> Sequence.map ~f:(fun idx -> { idx; arr = offsets; type_ = t })
  | None -> Sequence.empty

let idx x = x.idx
