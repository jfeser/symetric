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
    | Empty
    | Sketch of sketch Program.t * Type.t list
  [@@deriving compare, equal, hash, sexp, yojson]

  let default = Concat
  let cost _ = 1

  let ret_type : _ -> Type.t = function
    | Concat | Class _ | Repeat_range | Sketch _ | Optional | Or | Empty -> Regex
    | Int _ -> Int

  let args_type : _ -> Type.t list = function
    | Concat | Or -> [ Regex; Regex ]
    | Optional -> [ Regex ]
    | Class _ | Int _ | Empty -> []
    | Repeat_range -> [ Regex; Int; Int ]
    | Sketch (_, ts) -> ts

  let arity op = List.length @@ args_type op
  let is_commutative = function Or -> true | _ -> false
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
end

module Value = struct
  type t = Int of int | Matches of Set.M(Int).t Map.M(Int).t list | Error
  [@@deriving compare, equal, hash, sexp]

  let default = Int (-1)

  module Ctx = struct
    type t = { input : (string * bool) list } [@@deriving sexp]

    let create input = { input }
  end

  let eval_empty (ctx : Ctx.t) =
    List.map ctx.input ~f:(fun (input, _) ->
        Iter.int_range ~start:0 ~stop:(String.length input)
        |> Iter.fold
             (fun m i -> Map.add_exn m ~key:i ~data:(Set.singleton (module Int) i))
             (Map.empty (module Int)))

  let eval_or x x' =
    List.map2_exn x x'
      ~f:
        (Map.merge ~f:(fun ~key:_ -> function
           | `Left x | `Right x -> Some x | `Both (x, x') -> Some (Set.union x x')))

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

  let eval_unmemoized (ctx : Ctx.t) (op : Op.t) args =
    match (op, args) with
    | Int x, [] -> Int x
    | Empty, [] -> Matches (eval_empty ctx)
    | Class (Single c), [] ->
        Matches
          (List.map ctx.input ~f:(fun (input, _) ->
               Iter.int_range ~start:0 ~stop:(String.length input - 1)
               |> Iter.filter (fun idx -> [%equal: char] input.[idx] c)
               |> Iter.fold
                    (fun m i ->
                      Map.add_exn m ~key:i ~data:(Set.singleton (module Int) (i + 1)))
                    (Map.empty (module Int))))
    | Class (Multi { mem; _ }), [] ->
        Matches
          (List.map ctx.input ~f:(fun (input, _) ->
               Iter.int_range ~start:0 ~stop:(String.length input - 1)
               |> Iter.filter (fun idx -> mem input.[idx])
               |> Iter.fold
                    (fun m i ->
                      Map.add_exn m ~key:i ~data:(Set.singleton (module Int) (i + 1)))
                    (Map.empty (module Int))))
    | (Or | Concat), ([ Error; _ ] | [ _; Error ]) -> Error
    | Or, [ Matches x; Matches x' ] -> Matches (eval_or x x')
    | Concat, [ Matches x; Matches x' ] ->
        Matches
          (List.map2_exn x x' ~f:(fun x x' ->
               Map.filter_map x ~f:(fun end_pos1 ->
                   let end_pos =
                     Iter.of_set end_pos1
                     |> Iter.filter_map (Map.find x')
                     |> Iter.fold Set.union (Set.empty (module Int))
                   in
                   if Set.is_empty end_pos then None else Some end_pos)))
    | Repeat_range, ([ Error; _; _ ] | [ _; Error; _ ] | [ _; _; Error ]) -> Error
    | Repeat_range, [ Matches _; Int min; Int max ] when max < min -> Error
    | Repeat_range, [ Matches ms; Int min; Int max ] ->
        Matches (eval_repeat_range ms min max)
    | Optional, [ Error ] -> Error
    | Optional, [ Matches x ] -> Matches (eval_or (eval_empty ctx) x)
    | _ -> raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)]

  let target_distance (ctx : Ctx.t) = function
    | Error -> 1.0
    | Int _ -> 1.0
    | Matches m ->
        let correct =
          List.fold2_exn ctx.input m ~init:0 ~f:(fun acc (s, is_pos) m ->
              match Map.find m 0 with
              | Some ends ->
                  let full_match = Set.mem ends (String.length s) in
                  if is_pos then if full_match then acc + 1 else acc
                  else if full_match then acc
                  else acc + 1
              | None -> if is_pos then acc else acc + 1)
        in
        1.0 -. (Float.of_int correct /. Float.of_int (List.length ctx.input))

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
    [%expect {| ("Program.eval (eval_unmemoized ctx) concat" (Matches (((0 (2)) (10 (12)))))) |}];
    let repeat = Op.(repeat_range (P.apply Op.num) 1 3) in
    print_s [%message (Program.eval (eval_unmemoized ctx) repeat : t)];
    [%expect {|
      ("Program.eval (eval_unmemoized ctx) repeat"
       (Matches
        (((0 (1 2 3)) (1 (2 3 4)) (2 (3 4 5)) (3 (4 5 6)) (4 (5 6 7)) (5 (6 7 8))
          (6 (7 8 9)) (7 (8 9)) (8 (9)) (10 (11 12 13)) (11 (12 13)) (12 (13)))))) |}];
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
    let opt = Op.optional (P.apply (Op.class_ '.')) in
    let repeat = Op.repeat_range (P.apply Op.num) 1 15 in
    let repeat_concat = Op.concat repeat opt in
    let repeat_val = (Program.eval (eval_unmemoized ctx) repeat : t) in
    let opt_val = (Program.eval (eval_unmemoized ctx) opt : t) in
    let repeat_concat_val = (Program.eval (eval_unmemoized ctx) repeat_concat : t) in
    print_s [%message (opt_val : t)];
    print_s [%message (repeat_val : t)];
    print_s [%message (repeat_concat_val : t)];
    print_s [%message (target_distance ctx repeat_concat_val : float)];
    [%expect {|
      (opt_val
       (Matches
        (((0 (0)) (1 (1)) (2 (2)) (3 (3)) (4 (4)) (5 (5)) (6 (6)) (7 (7)) (8 (8))
          (9 (9)) (10 (10)) (11 (11)) (12 (12)) (13 (13)) (14 (14)) (15 (15))))))
      (repeat_val
       (Matches
        (((0 (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
          (1 (2 3 4 5 6 7 8 9 10 11 12 13 14 15))
          (2 (3 4 5 6 7 8 9 10 11 12 13 14 15)) (3 (4 5 6 7 8 9 10 11 12 13 14 15))
          (4 (5 6 7 8 9 10 11 12 13 14 15)) (5 (6 7 8 9 10 11 12 13 14 15))
          (6 (7 8 9 10 11 12 13 14 15)) (7 (8 9 10 11 12 13 14 15))
          (8 (9 10 11 12 13 14 15)) (9 (10 11 12 13 14 15)) (10 (11 12 13 14 15))
          (11 (12 13 14 15)) (12 (13 14 15)) (13 (14 15)) (14 (15))))))
      (repeat_concat_val
       (Matches
        (((0 (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
          (1 (2 3 4 5 6 7 8 9 10 11 12 13 14 15))
          (2 (3 4 5 6 7 8 9 10 11 12 13 14 15)) (3 (4 5 6 7 8 9 10 11 12 13 14 15))
          (4 (5 6 7 8 9 10 11 12 13 14 15)) (5 (6 7 8 9 10 11 12 13 14 15))
          (6 (7 8 9 10 11 12 13 14 15)) (7 (8 9 10 11 12 13 14 15))
          (8 (9 10 11 12 13 14 15)) (9 (10 11 12 13 14 15)) (10 (11 12 13 14 15))
          (11 (12 13 14 15)) (12 (13 14 15)) (13 (14 15)) (14 (15))))))
      ("target_distance ctx repeat_concat_val" 0) |}]

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
    let intersect_size m m' =
      Map.merge m m' ~f:(fun ~key:_ -> function
        | `Left _ | `Right _ -> None | `Both (s, s') -> Some (Set.length (Set.inter s s')))
      |> Map.data
      |> List.sum (module Int) ~f:Fun.id
      |> Float.of_int
    in
    let union_size m m' =
      Map.merge m m' ~f:(fun ~key:_ -> function
        | `Left s | `Right s -> Some (Set.length s)
        | `Both (s, s') -> Some (Set.length (Set.union s s')))
      |> Map.data
      |> List.sum (module Int) ~f:Fun.id
      |> Float.of_int
    in
    match (v, v') with
    | Int x, Int x' -> if x = x' then 0. else 1.
    | Matches ms, Matches ms' ->
        let similarity =
          List.map2_exn ms ms' ~f:(fun m m' ->
              let d = intersect_size m m' /. union_size m m' in
              d *. d)
          |> Iter.of_list |> Iter.mean |> Option.value ~default:0.
        in
        1. -. similarity
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
    print_s [%message (distance c1 c2 : float) (distance c1 c3 : float)];
    [%expect {| (("distance c1 c2" 1) ("distance c1 c3" 0.75)) |}]

  let pp = Fmt.nop
end

let serialize = [%sexp_of: Op.t Program.t]
let parse = [%of_sexp: Op.t Program.t]

let load_bench ch =
  let open Option.Let_syntax in
  In_channel.input_lines ch |> Iter.of_list |> Iter.map String.strip
  |> Iter.drop_while (fun line -> not ([%equal: string] line "// examples"))
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
