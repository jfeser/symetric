open! Core
open Utils

module Bind = struct
  module T = struct
    type t = string [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let of_string = Fun.id
end

type nonterm = string [@@deriving compare, hash, sexp]

type 'n term =
  | Nonterm of 'n
  | App of string * 'n term list
  | As of 'n term * Bind.t
[@@deriving compare, hash, sexp]

let to_preorder t =
  let i = ref (-1) in
  let rec conv = function
    | As (t, n) -> As (conv t, n)
    | App (f, ts) -> App (f, List.map ts ~f:conv)
    | Nonterm n ->
        incr i;
        Nonterm (n, !i)
  in
  conv t

let rec bindings = function
  | Nonterm _ -> []
  | App (_, ts) -> List.concat_map ts ~f:bindings
  | As (t, n') -> (n', t) :: bindings t

let rec find_binding n = function
  | Nonterm _ -> None
  | App (_, ts) -> List.find_map ts ~f:(find_binding n)
  | As (t, n') -> if [%compare.equal: Bind.t] n n' then Some t else None

let rec non_terminals = function
  | Nonterm x -> [ x ]
  | App (_, ts) -> List.concat_map ~f:non_terminals ts
  | As (t, _) -> non_terminals t

let with_holes ?fresh term =
  let fresh = Option.value fresh ~default:(Fresh.create ()) in
  let term = to_preorder term in
  let holes =
    non_terminals term
    |> List.map ~f:(fun (sym, idx) -> (sym, idx, Fresh.name fresh "x%d"))
  in
  let holes_ctx =
    List.map holes ~f:(fun (sym, idx, id) -> (idx, App (id, [])))
    |> Map.of_alist_exn (module Int)
  in
  let rec rename = function
    | Nonterm (_, idx) -> Map.find_exn holes_ctx idx
    | App (f, ts) -> App (f, List.map ts ~f:rename)
    | As (t, n) -> As (rename t, n)
  in
  (rename term, holes)

module Untyped_term = struct
  module T = struct
    type t = nonterm term [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let rec pp fmt = function
    | Nonterm x -> Fmt.fmt "%s" fmt x
    | App (func, args) -> Fmt.fmt "@[<hov 2>%s(%a)@]" fmt func pp_args args
    | As (term, name) -> Fmt.fmt "@[%a as %s@]" fmt pp term name

  and pp_args args = Fmt.(iter ~sep:comma (fun f l -> List.iter ~f l) pp args)

  let rec size = function
    | Nonterm _ -> 1
    | App (_, ts) -> 1 + List.sum (module Int) ~f:size ts
    | As (t, _) -> size t

  let n_holes t = non_terminals t |> List.length

  let rec to_string = function
    | Nonterm x -> x
    | App (f, []) -> f
    | App (f, xs) ->
        List.map xs ~f:to_string |> String.concat ~sep:", "
        |> sprintf "%s(%s)" f
    | As (t, n) -> sprintf "%s as %s" (to_string t) n

  let rec map ?(nonterm = fun x -> Nonterm x) ?(app = fun n ts -> App (n, ts))
      ?(as_ = fun t n -> As (t, n)) = function
    | Nonterm t -> nonterm t
    | App (n, ts) ->
        let ts = List.map ts ~f:(map ~nonterm ~app ~as_) in
        app n ts
    | As (t, n) -> as_ (map ~nonterm ~app ~as_ t) n
end

module Term = struct
  type 's t = Untyped_term.t [@@deriving compare, hash, sexp]

  open Untyped_term

  let nonterm n = Nonterm n

  let app n ts = App (n, ts)

  let as_ t n = As (t, n)

  let rec load = function
    | Sexp.List (Atom f :: args) -> app f @@ List.map args ~f:load
    | Atom x -> app x []
    | _ -> failwith "unexpected sexp"

  let non_terminals = non_terminals

  let size = size

  let n_holes = n_holes

  let to_string = to_string

  let with_holes = with_holes

  let map = map

  let bindings = bindings
end

module Rule = struct
  type 's t = {
    lhs : nonterm;
    rhs : Untyped_term.t;
    sem : 's list; [@sexp.omit_nil]
  }
  [@@deriving compare, hash, sexp]

  let pp fmt { lhs; rhs; _ } =
    Fmt.fmt "@[<hov 2>@[%s@]@ ->@ @[%a@]@]" fmt lhs Untyped_term.pp rhs

  let lhs { lhs; _ } = lhs

  let rhs { rhs; _ } = rhs

  let semantics { sem; _ } = sem

  let create lhs rhs sem = { lhs; rhs; sem }

  let of_tuple (lhs, rhs) = create lhs rhs []
end

type 's t = 's Rule.t list [@@deriving compare, sexp]

open Untyped_term

let of_list = List.map ~f:Rule.of_tuple

let rhs g s =
  List.filter_map g ~f:(fun r ->
      if String.(s = Rule.lhs r) then Some (Rule.rhs r) else None)

let lhs g =
  List.map g ~f:Rule.lhs |> List.dedup_and_sort ~compare:[%compare: nonterm]

let rec product = function
  | [] -> []
  | [ s ] -> List.map ~f:(fun x -> [ x ]) s
  | s :: ss ->
      product ss
      |> List.concat_map ~f:(fun xs -> List.map s ~f:(fun x -> x :: xs))

let inline sym g =
  let rh_sides = rhs g sym in
  let rec subst_all = function
    | Nonterm x as t ->
        if [%compare.equal: nonterm] sym x then rh_sides else [ t ]
    | App (_, []) as t -> [ t ]
    | App (f, ts) ->
        List.map ts ~f:subst_all |> product
        |> List.map ~f:(fun ts -> App (f, ts))
    | As (t, n) -> List.map (subst_all t) ~f:(fun t' -> As (t', n))
  in
  List.concat_map g ~f:(fun r ->
      subst_all r.rhs |> List.map ~f:(fun rhs -> { r with rhs }))

let weighted_random ?(state = Random.State.default) l =
  if List.length l <= 0 then failwith "Selecting from an empty list";
  let x =
    Random.State.float state 1.0
    *. List.sum (module Float) l ~f:(fun (w, _) -> w)
  in
  let rec loop x = function
    | [] -> failwith "BUG: Weight did not decrease enough"
    | (w, e) :: ws ->
        let x = x -. w in
        if Float.(x <= 0.0) then e else loop x ws
  in
  loop x l

(* See: https://eli.thegreenplace.net/2010/01/28/generating-random-sentences-from-a-context-free-grammar *)
let sample ?(state = Random.State.default) ?(factor = 0.01) symbol g =
  let rec sample pcount sym =
    let random_prod =
      rhs g sym
      |> List.map ~f:(fun p ->
             let w =
               Map.find pcount p
               |> Option.map ~f:(fun c -> factor *. float c)
               |> Option.value ~default:1.0
             in
             (w, p))
      |> weighted_random ~state
    in
    let pcount =
      Map.update pcount random_prod ~f:(function Some c -> c + 1 | None -> 1)
    in
    Term.map random_prod ~nonterm:(sample pcount)
  in
  sample (Map.empty (module Untyped_term)) symbol

let sample_seq ?state ?factor symbol g =
  Sequence.unfold ~init:() ~f:(fun () ->
      Some (sample ?state ?factor symbol g, ()))
