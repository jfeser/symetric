module Make (A : Abs_intf.S) = struct
  module T = struct
    type t = A.t list [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let create = List.dedup_and_sort ~compare:[%compare: A.t]

  let lift x = [ x ]

  let lub a b = create (a @ b)

  let glb a b =
    List.concat_map a ~f:(fun x -> List.map b ~f:(fun y -> A.glb x y)) |> create

  let contains a c = List.exists a ~f:(fun a' -> A.contains a' c)
end
