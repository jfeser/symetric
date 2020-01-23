open! Core
open Utils

module Term = struct
  module T = struct
    type t = Nonterm of string | App of string * t list
    [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let rec size = function
    | Nonterm _ -> 1
    | App (_, ts) -> 1 + List.sum (module Int) ~f:size ts

  let rec non_terminals = function
    | Nonterm x -> [ x ]
    | App (_, ts) -> List.concat_map ~f:non_terminals ts

  let n_holes t = non_terminals t |> List.length

  let rec to_string = function
    | Nonterm x -> x
    | App (f, []) -> f
    | App (f, xs) ->
        List.map xs ~f:to_string |> String.concat ~sep:", "
        |> sprintf "%s(%s)" f

  let with_holes ?fresh ~equal t =
    let fresh = Option.value fresh ~default:(Fresh.create ()) in
    let nt = non_terminals t in
    let holes = ref [] in
    let rec rename = function
      | Nonterm v ->
          if List.mem nt v ~equal then (
            let v' = v ^ Fresh.name fresh "%d" in
            holes := (v, v') :: !holes;
            App (v', []) )
          else Nonterm v
      | App (f, ts) -> App (f, List.map ts ~f:rename)
    in
    let t' = rename t in
    (t', !holes)

  let rec map ?(nonterm = fun x -> Nonterm x) ?(app = fun n ts -> App (n, ts)) t
      =
    (* print_endline (to_string t); *)
    match t with
    | Nonterm t -> nonterm t
    | App (n, ts) ->
        let ts = List.map ts ~f:(map ~nonterm ~app) in
        app n ts
end

type nonterm = string [@@deriving compare, sexp]

type t = (nonterm * Term.t) list [@@deriving compare, sexp]

let rhs g s =
  List.filter_map g ~f:(fun (s', t) -> if String.(s = s') then Some t else None)

let non_terminals g =
  List.map g ~f:(fun (x, _) -> x)
  |> List.dedup_and_sort ~compare:[%compare: nonterm]

let rec product = function
  | [] -> []
  | [ s ] -> List.map ~f:(fun x -> [ x ]) s
  | s :: ss ->
      product ss
      |> List.concat_map ~f:(fun xs -> List.map s ~f:(fun x -> x :: xs))

let inline sym g =
  let rhs =
    List.filter_map g ~f:(fun (s, r) ->
        if [%compare.equal: nonterm] sym s then Some r else None)
  in
  let rec subst_all = function
    | Term.Nonterm x as t ->
        if [%compare.equal: nonterm] sym x then rhs else [ t ]
    | App (_, []) as t -> [ t ]
    | App (f, ts) ->
        List.map ts ~f:subst_all |> product
        |> List.map ~f:(fun ts -> Term.App (f, ts))
  in
  List.concat_map g ~f:(fun (lhs, rhs) ->
      subst_all rhs |> List.map ~f:(fun rhs' -> (lhs, rhs')))

let with_holes ?fresh = Term.with_holes ?fresh ~equal:[%compare.equal: nonterm]

let productions g sym =
  List.filter g ~f:(fun (sym', _) -> [%compare.equal: nonterm] sym sym')
  |> List.map ~f:(fun (_, prod) -> prod)

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
      productions g sym
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
  sample (Map.empty (module Term)) symbol

let sample_seq ?state ?factor symbol g =
  Sequence.unfold ~init:() ~f:(fun () ->
      Some (sample ?state ?factor symbol g, ()))

(* let shuffle ?(state = Random.State.default) a =
 *   let n = Array.length a in
 *   for i = 0 to n - 2 do
 *     let j = Random.State.int_incl state i (n - 1) in
 *     Array.swap a i j
 *   done
 * 
 * let shuffle_list ?(state = Random.State.default) l =
 *   let a = Array.of_list l in
 *   shuffle ~state a;
 *   Array.to_list a *)

(* let rec sample ?(state = Random.State.default) g symbol n =
 *   if n <= 0 then None
 *   else
 *     productions g symbol |> shuffle_list ~state
 *     |> List.find_map ~f:(fun body ->
 *            let exception Failed in
 *            let size =
 *              Term.size body - (Term.non_terminals body |> List.length)
 *            in
 *            try
 *              Term.map body ~nonterm:(fun sym ->
 *                  match sample ~state g sym (n - size) with
 *                  | Some t -> t
 *                  | None -> raise Failed)
 *              |> Option.some
 *            with Failed -> None) *)

(* let sum init n ~f =
 *   let rec loop acc k = if k > n then acc else loop (f k) (k + 1) in
 *   loop init
 * 
 * module Conv = struct
 *   let ( * ) g1 g2 =
 *     fun n -> sum 0 n ~f:(fun k -> g1 k * g2 (n - k) )
 * end
 * 
 * let sample g n =
 *   let r = non_terminals g |> List.length in
 * 
 *   (\* Fill the lookup table. *\)
 *   let a = Array.create ~len:r 0 in
 *   set (0, a) 1.0;
 * 
 *   for t = 1 to n do
 *     for s = t downto 0 do
 *       let rec loop i d =
 *         if i < r then
 *           for ai = s - d downto 0 do
 *             a.(i - 1) <- ai;
 *             loop (i + 1) (d + ai)
 *           done
 *         else
 *           (a.(i - 1) <- s - d; compute (t, a)) *)
