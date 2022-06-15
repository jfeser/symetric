open Std

let memoize = false

module Type = struct
  type t = Int | Regex [@@deriving compare, equal, hash, sexp, yojson]

  let default = Int
  let output = Regex
  let pp fmt x = Sexp.pp fmt @@ [%sexp_of: t] x
end

module Op = struct
  type sketch = Op of t | Arg of int

  and t =
    | Int of int
    | Concat
    | Class of string
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

  let int x = Apply (Int x, [])
end

module Value = struct
  type t = Int of int | Matches of Set.M(Int).t Map.M(Int).t list | Error
  [@@deriving compare, equal, hash, sexp]

  let default = Int (-1)

  module Ctx = struct
    type t = { input : (string * bool) list }

    let create input = { input }
  end

  let eval_empty (ctx : Ctx.t) =
    List.map ctx.input ~f:(fun (input, _) ->
        Iter.int_range ~start:0 ~stop:(String.length input - 1)
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
    | Class x, [] ->
        Matches
          (List.map ctx.input ~f:(fun (input, _) ->
               Iter.int_range ~start:0 ~stop:(String.length input - 1)
               |> Iter.filter (fun idx -> String.contains x (String.get input idx))
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
    | Repeat_range, [ Matches _; Int min; Int max ] when max < min -> Error
    | Repeat_range, [ Matches ms; Int min; Int max ] ->
        Matches (eval_repeat_range ms min max)
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
    print_s [%message (eval_unmemoized ctx (Class ".") [] : t)];
    [%expect {| ("eval_unmemoized ctx (Class \".\") []" (Matches (((9 (10)))))) |}];
    print_s [%message (eval_unmemoized ctx (Class "1") [] : t)];
    [%expect
      {| ("eval_unmemoized ctx (Class \"1\") []" (Matches (((0 (1)) (10 (11)))))) |}];
    let concat =
      Program.(Apply (Op.Concat, [ Apply (Class "1", []); Apply (Class "2", []) ]))
    in
    print_s [%message (Program.eval (eval_unmemoized ctx) concat : t)];
    [%expect {||}];
    let repeat =
      Program.(
        Apply
          ( Op.Repeat_range,
            [ Apply (Class "123456789", []); Apply (Int 1, []); Apply (Int 3, []) ] ))
    in
    print_s [%message (Program.eval (eval_unmemoized ctx) repeat : t)];
    [%expect {||}];
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
    let repeat =
      Program.(
        Apply
          ( Op.Concat,
            [
              Apply
                ( Repeat_range,
                  [
                    Apply (Class "0123456789", []); Apply (Int 1, []); Apply (Int 15, []);
                  ] );
              Apply
                ( Optional,
                  [
                    Apply
                      ( Concat,
                        [
                          Apply (Class ".", []);
                          Apply
                            ( Repeat_range,
                              [
                                Apply (Class "0123456789", []);
                                Apply (Int 1, []);
                                Apply (Int 3, []);
                              ] );
                        ] );
                  ] );
            ] ))
    in
    let repeat_val = (Program.eval (eval_unmemoized ctx) repeat : t) in
    print_s [%message (repeat_val : t)];
    print_s [%message (target_distance ctx repeat_val : float)]

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
    let c1 = eval_unmemoized ctx (Class "1") [] in
    let c2 = eval_unmemoized ctx (Class "2") [] in
    let c3 = eval_unmemoized ctx (Class "12") [] in
    print_s [%message (distance c1 c2 : float) (distance c1 c3 : float)]

  let pp = Fmt.nop
end

let serialize = [%sexp_of: Op.t Program.t]
let parse = [%of_sexp: Op.t Program.t]
