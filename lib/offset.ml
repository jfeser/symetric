type t = {
  idx : int;
  arr : float array; [@compare.ignore]
  type_ : Offset_type.t;
}
[@@deriving compare, hash, sexp]

type ctx = float array Map.M(Offset_type).t [@@deriving sexp_of]

let of_bench bench =
  let ctx =
    bench.Bench.ops
    |> List.filter_map ~f:(function
         | Ast0.Op.Offset o -> Some (o.Bench0.type_, o.offset)
         | _ -> None)
    |> Map.of_alist_multi (module Offset_type)
    |> Map.map ~f:(fun offsets ->
           List.dedup_and_sort ~compare:[%compare: float] offsets
           |> List.to_array)
  in
  let of_offset (o : Bench0.offset) =
    let type_ = o.type_ in
    let arr = Map.find_exn ctx type_ in
    let idx =
      Option.value_exn ~message:"could not find offset"
        (Array.binary_search arr ~compare:[%compare: float] `First_equal_to
           o.offset)
    in
    { idx; arr; type_ }
  in
  let bench =
    {
      bench with
      ops =
        List.map bench.ops ~f:(function
          | Offset o -> Ast0.Op.Offset (of_offset o)
          | (Union | Inter | Sub | Sphere _ | Cylinder _ | Cuboid _) as op -> op);
    }
  in
  (bench, ctx)

let of_list t l =
  List.map l ~f:(fun o -> (t, o))
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
