module Make (A : Abs_intf.S) = struct
  module T = struct
    type t = A.t list [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let to_list = Fun.id

  let of_list a =
    List.filter a ~f:(fun v ->
        not
          (List.exists a ~f:(fun v' ->
               (not ([%compare.equal: A.t] v v')) && A.is_subset v ~of_:v')))
    |> List.sort ~compare:[%compare: A.t]

  let top = [ A.top ]

  let bot = []

  let is_subset a ~of_:a' =
    List.for_all a ~f:(fun x ->
        List.exists a' ~f:(fun x' -> A.is_subset x ~of_:x'))

  let lub a b = of_list (a @ b)

  let glb a b =
    List.concat_map a ~f:(fun x -> List.map b ~f:(fun y -> A.glb x y))
    |> of_list

  let contains a c = List.exists a ~f:(fun a' -> A.contains a' c)

  let lift x = [ x ]
end
