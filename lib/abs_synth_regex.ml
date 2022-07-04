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

  type t = Match_at of (int * int) | Max_match_is_at_most of int
  [@@deriving compare, equal, hash, sexp]

  let cost = function `Concrete _ -> 5 | _ -> 1
  let pp fmt x = Sexp.pp_hum fmt @@ [%sexp_of: t] x

  let all_matches ms f =
    let n = Bitarray.Blocked_matrix.dim ms in
    for i = 0 to n - 1 do
      for j = i to n - 1 do
        if Bitarray.Blocked_matrix.get ms i j then f (Match_at (i, j))
      done
    done

  let lift = function
    | Value.Int _ -> Iter.empty
    | Value.Matches { value = [ ms ] } ->
        let mx = Iter.to_list @@ all_matches ms in
        let p =
          Iter.of_list mx
          |> Iter.filter_map (function Match_at (i, j) -> Some (j - i) | _ -> None)
          |> Iter.max
          |> Option.map ~f:(fun l -> [ Max_match_is_at_most l ])
          |> Option.value ~default:[]
        in
        Iter.of_list (mx @ p)
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
    | `Concrete c -> `Concrete c :: (Iter.to_list @@ Iter.map (fun p -> `Pred p) @@ lift c)
    | `Pred (Max_match_is_at_most x) ->
        List.range x 100 |> List.map ~f:(fun x -> `Pred (Max_match_is_at_most x))
    | `Pred (Match_at _) as p -> [ p ]
    | `True -> [ `True ]
    | `False -> [ `False ]

  let implies ps ps' =
    Set.is_subset
      (Set.of_list (module P) @@ List.concat_map ~f:complete ps')
      ~of_:(Set.of_list (module P) @@ List.concat_map ~f:complete ps)

  let eval p c =
    match (p, c) with
    | Match_at (i, j), Value.Matches { value = [ ms ]; _ } ->
        Bitarray.Blocked_matrix.get ms i j
    | Max_match_is_at_most k, Value.Matches { value = [ ms ]; _ } ->
        all_matches ms
        |> Iter.for_all (function Match_at (i, j) -> j - i <= k | _ -> true)
    | _ -> false

  let transfer_not _ = []

  let transfer_optional input = function
    | `Pred (Match_at _ as p) ->
        `Pred p :: List.init (String.length input) ~f:(fun i -> `Pred (Match_at (i, i)))
    | _ -> []

  let transfer_and a b =
    match (a, b) with
    | `Pred (Match_at m), `Pred (Match_at m') when [%equal: int * int] m m' ->
        [ `Pred (Match_at m) ]
    | `Pred (Match_at (i, j)), `Concrete c | `Concrete c, `Pred (Match_at (i, j)) -> (
        match c with
        | Value.Matches { value = [ ms ] } when Bitarray.Blocked_matrix.get ms i j ->
            [ `Pred (Match_at (i, j)) ]
        | _ -> [])
    | `Pred (Max_match_is_at_most m), `Pred (Max_match_is_at_most m') ->
        [ `Pred (Max_match_is_at_most (min m m')) ]
    | _ -> []

  let transfer_or input a b =
    match (a, b) with
    | `Pred (Match_at m), `True | `True, `Pred (Match_at m) -> [ `Pred (Match_at m) ]
    | `Concrete c, `True | `True, `Concrete c -> (
        match c with
        | Value.Matches { value = [ ms ] } ->
            let ret = ref [] in
            for i = 0 to String.length input do
              for j = i to String.length input do
                if Bitarray.Blocked_matrix.get ms i j then
                  ret := `Pred (Match_at (i, j)) :: !ret
              done
            done;
            !ret
        | _ -> [])
    | `Pred (Max_match_is_at_most m), `Pred (Max_match_is_at_most m') ->
        [ `Pred (Max_match_is_at_most (max m m')) ]
    | _ -> []

  let transfer_concat input a b =
    match (a, b) with
    | `Pred (Match_at (i, j)), `Pred (Match_at (j', k)) when j = j' ->
        [ `Pred (Match_at (i, k)) ]
    | `Pred (Max_match_is_at_most m), `Pred (Max_match_is_at_most m') ->
        [ `Pred (Max_match_is_at_most (min (String.length input) (m + m'))) ]
    | _ -> []

  let transfer_repeat input a b =
    match (a, b) with
    | `Pred (Max_match_is_at_most x), `Concrete (Value.Int i) ->
        [ `Pred (Max_match_is_at_most (min (String.length input) (x * i.value))) ]
    | _ -> []

  let transfer_repeat_at_least _ _ _ = []

  let transfer_repeat_range input a b c =
    match (a, b, c) with
    | `Pred (Max_match_is_at_most x), _, `Concrete (Value.Int i) ->
        [ `Pred (Max_match_is_at_most (min (String.length input) (x * i.value))) ]
    | _, `Concrete (Value.Int i), `Concrete (Value.Int j) when i.value > j.value ->
        [ `False ]
    | _ -> []

  let transfer ctx example (op : Op.t) args =
    let all_concrete_m =
      List.map args ~f:(function `Concrete c -> Some c | _ -> None) |> Option.all
    in
    match all_concrete_m with
    | Some args ->
        let c = Value.eval { ctx with input = [ example ] } op args in
        if Value.is_error c then [ `False ]
        else
          `True :: `Concrete c :: (Iter.to_list @@ Iter.map (fun p -> `Pred p) @@ lift c)
    | None -> (
        let input, _ = example in
        match (op, args) with
        | Not, [ a ] -> transfer_not a
        | Optional, [ a ] -> transfer_optional input a
        | And, [ a; b ] -> transfer_and a b
        | Or, [ a; b ] -> transfer_or input a b
        | Concat, [ a; b ] -> transfer_concat input a b
        | Repeat, [ a; b ] -> transfer_repeat input a b
        | Repeat_at_least, [ a; b ] -> transfer_repeat_at_least input a b
        | Repeat_range, [ a; b; c ] -> transfer_repeat_range input a b c
        | _ -> failwith "unexpected arguments")
end

module Regex = struct
  include Regex

  module Type = struct
    include Type

    let output = Regex
  end

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
  @@ return (fun () ->
         let exs = Regex_bench.load_examples In_channel.stdin in
         let check_abs_example (lhs, rhs) abs =
           Set.for_all abs ~f:(function
             | `True -> true
             | `Concrete (Value.Matches { value = [ ms ] }) ->
                 Bool.(Bitarray.Blocked_matrix.get ms 0 (String.length lhs) = rhs)
             | `Pred (Abs_value.Match_at (0, j)) -> j = String.length lhs
             | `Pred (Abs_value.Max_match_is_at_most k) -> rhs && String.length lhs <= k
             | _ -> false)
         in
         let check_example p (lhs, rhs) =
           match Program.eval (Value.eval (Value.Ctx.create [ (lhs, rhs) ] 0)) p with
           | Value.Matches { value = ms :: _ } ->
               Bool.(Bitarray.Blocked_matrix.get ms 0 (String.length lhs) = rhs)
           | _ -> false
         in
         Synth.synth check_abs_example check_example (Value.Ctx.create exs 0) exs
           (Op.default_operators 15))
