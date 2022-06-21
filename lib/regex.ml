open Std
module P = Program

let memoize = false

module Type = struct
  type t = Int | Regex [@@deriving compare, equal, hash, sexp, yojson]

  let default = Int
  let output = Regex
  let pp fmt x = Sexp.pp fmt @@ [%sexp_of: t] x
end

module Op = struct
  type char_class =
    | Single of char
    | Multi of { name : string; mem : char -> bool [@ignore] [@opaque] }
  [@@deriving compare, equal, hash, sexp, yojson]

  type sketch = Op of t | Arg of int

  and t =
    | Int of int
    | Concat
    | Class of char_class
    | Repeat_range
    | Optional
    | Or
    | And
    | Not
    | Empty
  [@@deriving compare, equal, hash, sexp, yojson]

  let default = Concat
  let cost _ = 1

  let ret_type : _ -> Type.t = function
    | Concat | Class _ | Repeat_range | Optional | Or | And | Not | Empty -> Regex
    | Int _ -> Int

  let args_type : _ -> Type.t list = function
    | Concat | Or | And -> [ Regex; Regex ]
    | Optional | Not -> [ Regex ]
    | Class _ | Int _ | Empty -> []
    | Repeat_range -> [ Regex; Int; Int ]

  let arity op = List.length @@ args_type op
  let is_commutative = function Or | And -> true | _ -> false
  let pp fmt x = Sexp.pp_hum fmt @@ [%sexp_of: t] x

  open Program.T

  let class_ c = Class (Single c)

  let alpha =
    Class
      (Multi
         {
           name = "<let>";
           mem = (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false);
         })

  let num =
    Class (Multi { name = "<num>"; mem = (function '0' .. '9' -> true | _ -> false) })

  let cap =
    Class (Multi { name = "<cap>"; mem = (function 'A' .. 'Z' -> true | _ -> false) })

  let low =
    Class (Multi { name = "<low>"; mem = (function 'a' .. 'z' -> true | _ -> false) })

  let any =
    Class (Multi { name = "<any>"; mem = (function ' ' .. '~' -> true | _ -> false) })

  let int x = Apply (Int x, [])
  let concat x x' = Apply (Concat, [ x; x' ])
  let repeat_range x l h = Apply (Repeat_range, [ x; int l; int h ])
  let optional x = Apply (Optional, [ x ])
  let ( || ) x x' = Apply (Or, [ x; x' ])
  let ( && ) x x' = Apply (And, [ x; x' ])
  let empty = Apply (Empty, [])
end

module Value = struct
  module M = Bitarray.Blocked_matrix

  type match_ = M.t [@@deriving compare, equal, hash]

  let sexp_of_match_ m =
    Iter.int_range ~start:0 ~stop:(M.dim m - 1)
    |> Iter.map (fun i ->
           let ends =
             Iter.int_range ~start:i ~stop:(M.dim m - 1)
             |> Iter.filter (fun j -> M.get m i j)
             |> Iter.to_list
           in
           (i, ends))
    |> Iter.filter (fun (_, ends) -> not (List.is_empty ends))
    |> Iter.to_list |> [%sexp_of: (int * int list) list]

  let match__of_sexp _ = assert false

  type t = Int of int | Matches of match_ list | Error
  [@@deriving compare, equal, hash, sexp]

  let default = Error

  module Ctx = struct
    type t = { input : (string * bool) list } [@@deriving sexp]

    let create input = { input }
  end

  let repeat_range matches l h =
    let rec process all_reps m =
      match Map.max_elt m with
      | None -> all_reps
      | Some (start, ends) ->
          (* find all the repeats beginning from `start` *)
          let this_reps = Map.singleton (module Int) 1 ends in
          let this_reps =
            Iter.of_set ends
            |> Iter.fold
                 (fun this_reps end_ ->
                   match Map.find all_reps end_ with
                   | Some reps ->
                       Map.fold reps ~init:this_reps
                         ~f:(fun ~key:count ~data:ends this_reps ->
                           Map.update this_reps (count + 1) ~f:(function
                             | Some ends' -> Set.union ends ends'
                             | None -> ends))
                   | None -> this_reps)
                 this_reps
          in
          let all_reps =
            if Map.is_empty this_reps then all_reps
            else Map.add_exn all_reps ~key:start ~data:this_reps
          in
          process all_reps (Map.remove m start)
    in
    let all_reps = process (Map.empty (module Int)) matches in

    let lower_bound = Incl l in
    let upper_bound = match h with Some b -> Incl b | None -> Unbounded in
    Map.map all_reps ~f:(fun reps ->
        Map.subrange reps ~lower_bound ~upper_bound
        |> Map.data
        |> Set.union_list (module Int))

  let eval_repeat_range ms min max =
    List.map ms ~f:(fun m -> repeat_range m min (Some max))

  let eval_unmemoized_open eval (ctx : Ctx.t) (op : Op.t) args =
    let fail () = raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)] in
    match (op, args) with
    | Int x, [] -> Int x
    | Empty, [] ->
        Matches
          (List.map ctx.input ~f:(fun (input, _) -> M.identity (String.length input + 1)))
    | Class (Single c), [] ->
        eval ctx
          (Op.Class
             (Multi { mem = (fun c' -> [%equal: Char.t] c c'); name = Char.to_string c }))
          []
    | Class (Multi { mem; _ }), [] ->
        Matches
          (List.map ctx.input ~f:(fun (input, _) ->
               Iter.int_range ~start:0 ~stop:(String.length input - 1)
               |> Iter.filter (fun idx -> mem input.[idx])
               |> Iter.fold
                    (fun m i -> M.set m i (i + 1) true)
                    (M.create (String.length input + 1) false)))
    | (Int _ | Empty | Class _), _ :: _ -> fail ()
    | (Not | Optional), [ Error ] -> Error
    | Not, [ Matches x ] -> Matches (List.map x ~f:M.O.lnot)
    | (Not | Optional), ([] | [ Int _ ] | _ :: _ :: _) -> fail ()
    | Optional, [ v ] -> eval ctx Or [ eval ctx Empty []; v ]
    | (And | Or | Concat), ([ Error; _ ] | [ _; Error ]) -> Error
    | (And | Or | Concat), ([] | [ _ ] | [ Int _; _ ] | [ _; Int _ ] | _ :: _ :: _ :: _)
      ->
        fail ()
    | Or, [ Matches x; Matches x' ] -> Matches (List.map2_exn x x' ~f:M.O.( lor ))
    | And, [ Matches x; Matches x' ] -> Matches (List.map2_exn x x' ~f:M.O.( land ))
    | Concat, [ Matches x; Matches x' ] ->
        Matches (List.map2_exn x x' ~f:(fun x x' -> M.O.(x * x')))
    | Repeat_range, ([ Error; _; _ ] | [ _; Error; _ ] | [ _; _; Error ]) -> Error
    | Repeat_range, [ _; Int min; Int max ] when max < min -> Error
    | Repeat_range, [ v; Int min; Int _max ] ->
        let rec repeat k = if k = 1 then v else eval ctx Concat [ v; repeat (k - 1) ] in
        (* let rec repeat_range k = *)
        (*   if k = max then repeat k else eval ctx Or [ repeat k; repeat_range (k + 1) ] *)
        (* in *)
        repeat min
        (* repeat_range min *)
    | ( Repeat_range,
        ( []
        | [ _ ]
        | [ _; _ ]
        | [ Int _; _; _ ]
        | [ _; Matches _; _ ]
        | [ _; _; Matches _ ]
        | _ :: _ :: _ :: _ :: _ ) ) ->
        fail ()

  let rec eval_unmemoized ctx op args = eval_unmemoized_open eval_unmemoized ctx op args

  let target_distance (ctx : Ctx.t) = function
    | Error -> 1.0
    | Int _ -> 1.0
    | Matches m ->
        let correct =
          List.fold2_exn ctx.input m ~init:0 ~f:(fun acc (s, is_pos) m ->
              let full_match = M.get m 0 (String.length s) in
              if [%equal: bool] is_pos full_match then acc + 1 else acc)
        in
        1.0 -. (Float.of_int correct /. Float.of_int (List.length ctx.input))

  let%expect_test "" =
    let ctx =
      Ctx.create
        [ ("1e", true); ("1", true); ("e", true); ("111e", true); ("eee11ee", true) ]
    in
    print_s
      [%message
        (Program.eval (eval_unmemoized ctx) Op.(concat (P.apply num) (P.apply alpha)) : t)];
    print_s
      [%message
        (Program.eval (eval_unmemoized ctx)
           Op.(concat (P.apply num) @@ concat (P.apply num) (P.apply alpha))
          : t)];
    [%expect
      {|
      ( "Program.eval (eval_unmemoized ctx)\
       \n  (let open Op in concat (P.apply num) (P.apply alpha))"
       (Matches (((0 (2))) () () ((2 (4))) ((4 (6))))))
      ( "Program.eval (eval_unmemoized ctx)\
       \n  (let open Op in\
       \n     (concat (P.apply num)) @@ (concat (P.apply num) (P.apply alpha)))"
       (Matches (() () () ((1 (4))) ((3 (6)))))) |}]

  let%expect_test "" =
    let ctx = Ctx.create [ ("1e", true); ("1", true); ("e", true); ("1.1e", true) ] in
    print_s
      [%message
        (Program.eval (eval_unmemoized ctx) Op.(P.apply num || P.apply alpha) : t)];
    [%expect
      {|
      ( "Program.eval (eval_unmemoized ctx)\
       \n  (let open Op in (P.apply num) || (P.apply alpha))"
       (Matches (((0 (1)) (1 (2))) ((0 (1))) ((0 (1))) ((0 (1)) (2 (3)) (3 (4)))))) |}]

  let%expect_test "" =
    let ctx = Ctx.create [ ("1", true); ("11", true); ("111", true); ("1111", true) ] in
    print_s
      [%message
        (Program.eval (eval_unmemoized ctx) Op.(repeat_range (P.apply num) 2 3) : t)];
    [%expect
      {|
      ( "Program.eval (eval_unmemoized ctx)\
       \n  (let open Op in repeat_range (P.apply num) 2 3)"
       (Matches (() ((0 (2))) ((0 (2)) (1 (3))) ((0 (2)) (1 (3)) (2 (4)))))) |}]

  let%expect_test "" =
    let input = "123456789.123" in
    let ctx = Ctx.create [ (input, true) ] in
    print_s [%message (eval_unmemoized ctx (Op.class_ '.') [] : t)];
    [%expect {| ("eval_unmemoized ctx (Op.class_ '.') []" (Matches (((9 (10)))))) |}];
    print_s [%message (eval_unmemoized ctx (Op.class_ '1') [] : t)];
    [%expect
      {| ("eval_unmemoized ctx (Op.class_ '1') []" (Matches (((0 (1)) (10 (11)))))) |}];
    let concat =
      Program.(
        Apply (Op.Concat, [ Apply (Op.class_ '1', []); Apply (Op.class_ '2', []) ]))
    in
    print_s [%message (Program.eval (eval_unmemoized ctx) concat : t)];
    [%expect
      {| ("Program.eval (eval_unmemoized ctx) concat" (Matches (((0 (2)) (10 (12)))))) |}];
    let repeat = Op.(repeat_range (P.apply Op.num) 1 3) in
    print_s [%message (Program.eval (eval_unmemoized ctx) repeat : t)];
    [%expect
      {|
      ("Program.eval (eval_unmemoized ctx) repeat"
       (Matches
        (((0 (1)) (1 (2)) (2 (3)) (3 (4)) (4 (5)) (5 (6)) (6 (7)) (7 (8)) (8 (9))
          (10 (11)) (11 (12)) (12 (13)))))) |}];
    let ctx =
      Ctx.create
        [
          (* ("123456789.123", true); *)
          (* ("123456789123456.12", true); *)
          (* ("12345.1", true); *)
          ("123456789123456", true)
          (* ("1234567891234567", false); *)
          (* ("123.1234", false); *)
          (* ("1.12345", false); *)
          (* (".1234", false); *);
        ]
    in
    let dot = P.apply (Op.class_ '.') in
    let empty = Op.empty in
    let opt = Op.optional dot in
    let repeat = Op.repeat_range (P.apply Op.num) 1 15 in
    let repeat_concat = Op.concat repeat opt in
    let repeat_val = (Program.eval (eval_unmemoized ctx) repeat : t) in
    let dot_val = (Program.eval (eval_unmemoized ctx) dot : t) in
    let empty_val = (Program.eval (eval_unmemoized ctx) empty : t) in
    let empty_or_dot_val = (Program.eval (eval_unmemoized ctx) Op.(empty || dot) : t) in
    let opt_val = (Program.eval (eval_unmemoized ctx) opt : t) in
    let repeat_concat_val = (Program.eval (eval_unmemoized ctx) repeat_concat : t) in
    print_s [%message (dot_val : t)];
    print_s [%message (empty_val : t)];
    print_s [%message (empty_or_dot_val : t)];
    print_s [%message (opt_val : t)];
    print_s [%message (repeat_val : t)];
    print_s [%message (repeat_concat_val : t)];
    print_s [%message (target_distance ctx repeat_concat_val : float)];
    [%expect
      {|
      (dot_val (Matches (())))
      (empty_val
       (Matches
        (((0 (0)) (1 (1)) (2 (2)) (3 (3)) (4 (4)) (5 (5)) (6 (6)) (7 (7)) (8 (8))
          (9 (9)) (10 (10)) (11 (11)) (12 (12)) (13 (13)) (14 (14)) (15 (15))))))
      (empty_or_dot_val
       (Matches
        (((0 (0)) (1 (1)) (2 (2)) (3 (3)) (4 (4)) (5 (5)) (6 (6)) (7 (7)) (8 (8))
          (9 (9)) (10 (10)) (11 (11)) (12 (12)) (13 (13)) (14 (14)) (15 (15))))))
      (opt_val
       (Matches
        (((0 (0)) (1 (1)) (2 (2)) (3 (3)) (4 (4)) (5 (5)) (6 (6)) (7 (7)) (8 (8))
          (9 (9)) (10 (10)) (11 (11)) (12 (12)) (13 (13)) (14 (14)) (15 (15))))))
      (repeat_val
       (Matches
        (((0 (1)) (1 (2)) (2 (3)) (3 (4)) (4 (5)) (5 (6)) (6 (7)) (7 (8)) (8 (9))
          (9 (10)) (10 (11)) (11 (12)) (12 (13)) (13 (14)) (14 (15))))))
      (repeat_concat_val
       (Matches
        (((0 (1)) (1 (2)) (2 (3)) (3 (4)) (4 (5)) (5 (6)) (6 (7)) (7 (8)) (8 (9))
          (9 (10)) (10 (11)) (11 (12)) (12 (13)) (13 (14)) (14 (15))))))
      ("target_distance ctx repeat_concat_val" 1) |}]

  let mk_eval_memoized () =
    let module Key = struct
      module T = struct
        type nonrec t = Op.t * t list [@@deriving compare, hash, sexp]
      end

      include T
      include Comparable.Make (T)
    end in
    let tbl = Hashtbl.create (module Key) in
    let find_or_eval (ctx : Ctx.t) op args =
      match Hashtbl.find tbl (op, args) with
      | Some v -> v
      | None ->
          let v = eval_unmemoized ctx op args in
          Hashtbl.set tbl ~key:(op, args) ~data:v;
          v
    in
    find_or_eval

  let eval = if memoize then mk_eval_memoized () else eval_unmemoized
  let is_error = function Error -> true | _ -> false

  let distance v v' =
    match (v, v') with
    | Int x, Int x' -> if x = x' then 0. else 1.
    | Matches ms, Matches ms' ->
        let distance =
          List.map2_exn ms ms' ~f:(fun m m' ->
              Bitarray.jaccard_distance (M.to_bitarray m) (M.to_bitarray m'))
          |> Iter.of_list |> Iter.mean |> Option.value ~default:0.
        in
        distance
    | _ -> 1.

  let%expect_test "" =
    let input = "123456789.123" in
    let ctx = Ctx.create [ (input, true) ] in
    let c1 = eval_unmemoized ctx (Op.class_ '1') [] in
    let c2 = eval_unmemoized ctx (Op.class_ '2') [] in
    let c3 =
      eval_unmemoized ctx
        (Op.Class
           (Multi { name = "12"; mem = (function '1' | '2' -> true | _ -> false) }))
        []
    in
    print_s [%message (c1 : t)];
    [%expect {| (c1 (Matches (((0 (1)) (10 (11)))))) |}];
    print_s [%message (c2 : t)];
    [%expect {| (c2 (Matches (((1 (2)) (11 (12)))))) |}];
    print_s [%message (c3 : t)];
    [%expect {| (c3 (Matches (((0 (1)) (1 (2)) (10 (11)) (11 (12)))))) |}];
    print_s [%message (distance c1 c2 : float) (distance c1 c3 : float)];
    [%expect {| (("distance c1 c2" 1) ("distance c1 c3" 0.5)) |}]

  let pp fmt = function
    | Matches m ->
        Sexp.pp_hum fmt
          ([%sexp_of: bool list] (List.map m ~f:(fun m -> M.get m 0 (M.dim m - 1))))
    | _ -> ()
end

let serialize = [%sexp_of: Op.t Program.t]
let parse = [%of_sexp: Op.t Program.t]

let load_bench ch =
  let open Option.Let_syntax in
  In_channel.input_lines ch |> Iter.of_list |> Iter.map String.strip
  |> Iter.drop_while (fun line ->
         not ([%equal: string] line "// examples" || [%equal: string] line "// example"))
  |> Iter.drop 1
  |> Iter.filter_map (fun line ->
         let len = String.length line in
         if len > 0 then
           let%map is_pos =
             match line.[len - 1] with '+' -> Some true | '-' -> Some false | _ -> None
           in
           (String.drop_suffix (String.drop_prefix line 1) 3, is_pos)
         else None)
  |> Iter.to_list
