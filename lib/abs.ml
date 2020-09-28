module T = struct
  type t = bool Map.M(Int).t [@@deriving compare, equal, hash, sexp]
end

include T
include Comparator.Make (T)

let top = Map.empty (module Int)

let pp : t Fmt.t =
  Fmt.using (fun m ->
      Map.to_alist m |> List.map ~f:(fun (k, v) -> (Bool.to_int v, k)))
  @@ Fmt.list ~sep:(Fmt.any " ")
  @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "_%d")

let graphviz_pp : t Fmt.t =
  Fmt.using (fun m ->
      Map.to_alist m |> List.map ~f:(fun (k, v) -> (Bool.to_int v, k)))
  @@ Fmt.list ~sep:(Fmt.any " ")
  @@ Fmt.pair ~sep:Fmt.nop Fmt.int (Fmt.fmt "<sub>%d</sub>")

let meet =
  Map.merge ~f:(fun ~key:_ -> function
    | `Left x | `Right x -> Some x
    | `Both (x, x') ->
        assert (Bool.(x = x'));
        Some x)

let is_subset_a s ~of_:s' =
  if Map.length s > Map.length s' then false
  else
    Map.fold2 s s' ~init:true ~f:(fun ~key:_ ~data acc ->
        acc
        &&
        match data with
        | `Left _ -> false
        | `Right _ -> true
        | `Both (x, x') -> Bool.(x = x'))

let lift s =
  Array.mapi s ~f:(fun i x -> (i, x))
  |> Array.to_list
  |> Map.of_alist_exn (module Int)

let union =
  Map.merge ~f:(fun ~key:_ -> function
    | `Both (x, x') -> Some (x || x')
    | `Left true | `Right true -> Some true
    | `Left false | `Right false -> None)

let inter =
  Map.merge ~f:(fun ~key:_ -> function
    | `Both (x, x') -> Some (x && x')
    | `Left false | `Right false -> Some false
    | `Left true | `Right true -> None)

let sub =
  Map.merge ~f:(fun ~key:_ -> function
    | `Both (x, x') -> Some (x && not x')
    | `Left false | `Right true -> Some false
    | `Left true | `Right false -> None)

let mem v i = Map.mem v i

let set m k v = Map.set m ~key:k ~data:v

let add m k v =
  let open Or_error in
  match Map.find m k with
  | Some v' ->
      if Bool.(v = v') then return m
      else error "Conflicting values for bit" k [%sexp_of: int]
  | None -> return @@ set m k v

let add_exn m k v = Or_error.ok_exn @@ add m k v

let contains a c = Map.for_alli a ~f:(fun ~key:i ~data:v -> Bool.(c.(i) = v))

let width = Map.length

let of_list_exn l = Map.of_alist_exn (module Int) l
