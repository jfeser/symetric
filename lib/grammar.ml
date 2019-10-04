open! Core

module Term = struct
  type t = Id of string | App of string * t list [@@deriving compare, sexp]

  let rec size = function
    | Id _ -> 1
    | App (_, ts) -> 1 + List.sum (module Int) ~f:size ts
end

type t = (string * Term.t) list

let rule_size (_, t) = Term.size t

let num_holes (lhs, rhs) =
  let rec num = function
    | Term.Id v -> if String.(v = lhs) then 1 else 0
    | App (_, ts) -> List.sum (module Int) ~f:num ts
  in
  num rhs

let non_terminals g =
  List.map g ~f:(fun (x, _) -> x)
  |> List.dedup_and_sort ~compare:[%compare: string]

let rec product = function
  | [] -> []
  | [ s ] -> List.map ~f:(fun x -> [ x ]) s
  | s :: ss ->
      product ss
      |> List.concat_map ~f:(fun xs -> List.map s ~f:(fun x -> x :: xs))

let inline sym g =
  let rhs =
    List.filter_map g ~f:(fun (s, r) ->
        if String.(sym = s) then Some r else None)
  in
  let rec subst_all = function
    | Term.Id x as t -> if String.(x = sym) then rhs else [ t ]
    | App (f, ts) ->
        List.map ts ~f:subst_all |> product
        |> List.map ~f:(fun ts -> Term.App (f, ts))
  in
  List.concat_map g ~f:(fun (lhs, rhs) ->
      subst_all rhs |> List.map ~f:(fun rhs' -> (lhs, rhs')))
