open Std
open Regex

module Abs_value
    (*  : *)
    (* Abstract_synth_example.Domain_pred_intf *)
    (*   with type concrete = Regex.Value.t *)
    (*    and type op = Regex.Op.t *)
    (*    and type ctx = Regex.Value.Ctx.t *)
    (*    and type example = string * bool *) =
struct
  type concrete = Regex.Value.t
  type op = Regex.Op.t
  type ctx = Regex.Value.Ctx.t
  type example = string * bool
  type measure = Longest_match [@@deriving compare, equal, hash, sexp]
  type bound = Lower | Upper [@@deriving compare, equal, hash, sexp]

  type t =
    | Match_at of (int * int * bool)
    | Max_match_is_at_most of int
    | Fills_hole of int
  [@@deriving compare, equal, hash, sexp]

  let cost = function `Concrete _ -> 10 | _ -> 1
  let pp fmt x = Sexp.pp_hum fmt @@ [%sexp_of: t] x

  let all_matches ms f =
    let n = Bitarray.Blocked_matrix.dim ms in
    for i = 0 to n - 1 do
      for j = i to n - 1 do
        f (Match_at (i, j, Bitarray.Blocked_matrix.get ms i j))
      done
    done

  let longest_match ms =
    all_matches ms
    |> Iter.filter_map (function Match_at (i, j, true) -> Some (j - i) | _ -> None)
    |> Iter.max

  let fills hole =
    Bitarray.to_list hole
    |> List.filter_mapi ~f:(fun i b -> if b then Some (`Pred (Fills_hole i)) else None)

  let lift = function
    | Value.Int _ -> Iter.empty
    | Value.Error -> Iter.singleton `False
    | Value.Matches { value = [ ms ]; holes } ->
        (* let mx = Iter.to_list @@ Iter.map (fun p -> `Pred p) @@ all_matches ms in *)
        let p =
          longest_match ms
          |> Option.map ~f:(fun l -> [ `Pred (Max_match_is_at_most l) ])
          |> Option.value ~default:[]
        in
        Iter.of_list ((* mx @ *) p @ fills holes)
    | v -> raise_s [%message "unexpected value" (v : Value.t)]

  module P = struct
    module T = struct
      type nonrec t = [ `Pred of t | `Concrete of Value.t | `True | `False ]
      [@@deriving compare, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  let complete = function
    | `Concrete c -> `Concrete c :: (Iter.to_list @@ lift c)
    | `Pred (Max_match_is_at_most x) ->
        List.range x 30 |> List.map ~f:(fun x -> `Pred (Max_match_is_at_most x))
    | (`Pred (Match_at _) | `Pred (Fills_hole _)) as p -> [ p ]
    | `True -> [ `True ]
    | `False -> [ `False ]

  let implies ps ps' =
    Set.is_subset
      (Set.of_list (module P) @@ List.concat_map ~f:complete ps')
      ~of_:(Set.of_list (module P) @@ List.concat_map ~f:complete ps)

  let eval p c =
    match (p, c) with
    | Match_at (i, j, b), Value.Matches { value = [ ms ]; _ } ->
        Bool.(Bitarray.Blocked_matrix.get ms i j = b)
    | Max_match_is_at_most k, Value.Matches { value = [ ms ]; _ } ->
        all_matches ms
        |> Iter.for_all (function Match_at (i, j, true) -> j - i <= k | _ -> true)
    | Fills_hole i, (Value.Matches { holes; _ } | Value.Int { holes; _ }) ->
        Bitarray.get holes i
    | _ -> false

  let transfer_not = function
    | `Pred (Match_at (i, j, b)) -> [ `Pred (Match_at (i, j, not b)) ]
    | `Pred (Fills_hole _) as p -> [ p ]
    | _ -> []

  let transfer_optional input = function
    | `True -> List.init (String.length input) ~f:(fun i -> `Pred (Match_at (i, i, true)))
    | `Pred (Match_at (_, _, true) as p) -> [ `Pred p ]
    | `Pred (Match_at (i, j, false) as p) -> if i = j then [] else [ `Pred p ]
    | `Pred (Fills_hole _) as p -> [ p ]
    | _ -> []

  let transfer_and a b =
    match (a, b) with
    | `Pred (Match_at (i, j, false)), `True | `True, `Pred (Match_at (i, j, false)) ->
        [ `Pred (Match_at (i, j, false)) ]
    | `Pred (Match_at (i, j, b)), `Pred (Match_at (i', j', b')) when i = i' && j = j' ->
        [ `Pred (Match_at (i, j, b && b')) ]
    | `Pred (Match_at (i, j, b)), `Concrete c | `Concrete c, `Pred (Match_at (i, j, b))
      -> (
        match c with
        | Value.Matches { value = [ ms ] } ->
            [ `Pred (Match_at (i, j, b && Bitarray.Blocked_matrix.get ms i j)) ]
        | _ -> [])
    | `Pred (Max_match_is_at_most m), `Pred (Max_match_is_at_most m') ->
        [ `Pred (Max_match_is_at_most (min m m')) ]
    | `True, (`Pred (Fills_hole _) as p) | (`Pred (Fills_hole _) as p), `True -> [ p ]
    | _ -> []

  let transfer_or input a b =
    match (a, b) with
    | `Pred (Match_at (i, j, true)), `True | `True, `Pred (Match_at (i, j, true)) ->
        [ `Pred (Match_at (i, j, true)) ]
    | `Pred (Match_at (i, j, b)), `Pred (Match_at (i', j', b')) when i = i' && j = j' ->
        [ `Pred (Match_at (i, j, b || b')) ]
    | `Concrete c, `True | `True, `Concrete c -> (
        match c with
        | Value.Matches { value = [ ms ] } ->
            let ret = ref [] in
            for i = 0 to String.length input do
              for j = i to String.length input do
                if Bitarray.Blocked_matrix.get ms i j then
                  ret := `Pred (Match_at (i, j, true)) :: !ret
              done
            done;
            !ret
        | _ -> [])
    | `Pred (Max_match_is_at_most m), `Pred (Max_match_is_at_most m') ->
        [ `Pred (Max_match_is_at_most (max m m')) ]
    | `True, (`Pred (Fills_hole _) as p) | (`Pred (Fills_hole _) as p), `True -> [ p ]
    | _ -> []

  let transfer_concat input a b =
    match (a, b) with
    | `Pred (Match_at (i, j, true)), `Pred (Match_at (j', k, true)) when j = j' ->
        [ `Pred (Match_at (i, k, true)) ]
    | `Concrete c, `Pred (Match_at (j, k, true)) -> (
        match c with
        | Value.Matches { value = [ ms ] } ->
            all_matches ms
            |> Iter.filter_map (function
                 | Match_at (i, j', true) when j = j' ->
                     Some (`Pred (Match_at (i, k, true)))
                 | _ -> None)
            |> Iter.to_list
        | _ -> [])
    | `Pred (Match_at (i, j, true)), `Concrete c -> (
        match c with
        | Value.Matches { value = [ ms ] } ->
            all_matches ms
            |> Iter.filter_map (function
                 | Match_at (j', k, true) when j = j' ->
                     Some (`Pred (Match_at (i, k, true)))
                 | _ -> None)
            |> Iter.to_list
        | _ -> [])
    | `Pred (Max_match_is_at_most m), `Pred (Max_match_is_at_most m') ->
        [ `Pred (Max_match_is_at_most (min (String.length input) (m + m'))) ]
    | `True, (`Pred (Fills_hole _) as p) | (`Pred (Fills_hole _) as p), `True -> [ p ]
    | _ -> []

  let transfer_repeat input a b =
    match (a, b) with
    | `Pred (Max_match_is_at_most x), `Concrete (Value.Int i) ->
        [ `Pred (Max_match_is_at_most (min (String.length input) (x * i.value))) ]
    | `True, (`Pred (Fills_hole _) as p) | (`Pred (Fills_hole _) as p), `True -> [ p ]
    | _ -> []

  let transfer_repeat_at_least a b =
    match (a, b) with
    | `True, (`Pred (Fills_hole _) as p) | (`Pred (Fills_hole _) as p), `True -> [ p ]
    | _ -> []

  let transfer_repeat_range input a b c =
    match (a, b, c) with
    | `Pred (Max_match_is_at_most x), _, `Concrete (Value.Int i) ->
        [ `Pred (Max_match_is_at_most (min (String.length input) (x * i.value))) ]
    | _, `Concrete (Value.Int i), `Concrete (Value.Int j) when i.value > j.value ->
        [ `False ]
    | `True, `True, (`Pred (Fills_hole _) as p)
    | `True, (`Pred (Fills_hole _) as p), `True
    | (`Pred (Fills_hole _) as p), `True, `True ->
        [ p ]
    | _ -> []

  let rec transfer preds ctx example (op : Op.t) args =
    let all_concrete_m =
      List.map args ~f:(function `Concrete c -> Some c | _ -> None) |> Option.all
    in
    match all_concrete_m with
    | Some args ->
        let c = Value.eval { ctx with input = [ example ] } op args in
        if Value.is_error c then [ `False ]
        else `True :: `Concrete c :: (Iter.to_list @@ lift c)
    | None -> (
        let input, _ = example in
        match (op, args) with
        | Not, [ a ] -> transfer_not a
        | Optional, [ a ] -> transfer_optional input a
        | And, [ a; b ] -> transfer_and a b
        | Or, [ a; b ] -> transfer_or input a b
        | Concat, [ a; b ] -> transfer_concat input a b
        | Repeat, [ a; b ] -> transfer_repeat input a b
        | Repeat_at_least, [ a; b ] -> transfer_repeat_at_least a b
        | Repeat_range, [ a; b; c ] -> transfer_repeat_range input a b c
        | Sketch sk, args ->
            if
              List.exists2_exn sk.arg_types args ~f:(fun (_, holes) p ->
                  match p with
                  | `Pred (Fills_hole i) -> not (Bitarray.get holes i)
                  | _ -> false)
            then [ `False ]
            else
              let value_preds =
                Set.to_list
                @@ transfer_sketch preds ctx example sk
                @@ List.map ~f:(Set.singleton (module P)) args
              in
              fills sk.ret_hole_mask @ value_preds
        | _ -> failwith "unexpected arguments")

  and transfer_list preds ctx example op args =
    List.map args ~f:(fun ps -> Iter.cons `True @@ Iter.of_set ps)
    |> Iter.list_product
    |> Iter.map (fun simple_args ->
           transfer preds ctx example op simple_args |> Iter.of_list)
    |> Iter.concat
    |> Iter.fold Set.add (Set.empty (module P))

  and transfer_sketch preds ctx example sk args =
    let preds_set = Set.of_list (module P) preds in
    Program.eval
      (fun op xs ->
        match op with
        | Op.Op op -> Set.inter preds_set (transfer_list preds ctx example op xs)
        | Hole i -> List.nth_exn args i)
      sk.term
end

module Regex = struct
  include Regex

  module Value = struct
    include Value

    type example = string * bool

    let eval ctx example op args = eval { ctx with input = [ example ] } op args
  end
end

module Synth = Abstract_synth_example.Make (Regex) (Abs_value)

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve regex problems with abstraction guided synthesis."
    [%map_open
      let sketch = flag "-sketch" (required string) ~doc:" regex sketch" in
      fun () ->
        let ctx, ops = Regex_bench.load_sketch_bench sketch In_channel.stdin in
        let check_abs_example (lhs, rhs) abs =
          Set.for_all abs ~f:(function
            | `True -> true
            | `False -> false
            | `Concrete (Value.Matches { value = [ ms ] }) ->
                Bool.(Bitarray.Blocked_matrix.get ms 0 (String.length lhs) = rhs)
            | `Concrete (Value.Matches { value = _ }) ->
                failwith "unexpected number of matches"
            | `Concrete Value.Error -> false
            | `Concrete (Value.Int _) -> failwith "wrong type of value"
            | `Pred (Abs_value.Match_at (i, j, b)) ->
                if i = 0 && j = String.length lhs then Bool.(b = rhs) else true
            | `Pred (Abs_value.Max_match_is_at_most k) ->
                if rhs && String.length lhs > k then false else true
            | `Pred (Fills_hole _) -> true)
        in
        let check_example p (lhs, rhs) =
          match Program.eval (Value.eval { ctx with input = [ (lhs, rhs) ] }) p with
          | Value.Matches { value = ms :: _; holes }
            when Bitarray.length holes = 0 || Bitarray.get holes 0 ->
              Bool.(Bitarray.Blocked_matrix.get ms 0 (String.length lhs) = rhs)
          | _ -> false
        in
        let examples = ctx.input in
        Synth.synth check_abs_example check_example ctx examples
          (Op.default_operators 15 @ ops)]
