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

  type t = Max_match_is_at_most of int | First_match_is_after of int | Fills_hole of int
  [@@deriving compare, equal, hash, sexp]

  let cost = function `Concrete _ -> 10 | _ -> 1
  let pp fmt x = Sexp.pp_hum fmt @@ [%sexp_of: t] x

  let all_matches ms f =
    let n = Bitarray.Blocked_matrix.dim ms in
    for i = 0 to n - 1 do
      for j = i to n - 1 do
        if Bitarray.Blocked_matrix.get ms i j then f (i, j)
      done
    done

  let longest_match ms = all_matches ms |> Iter.map (function i, j -> j - i) |> Iter.max
  let first_match ms = all_matches ms |> Iter.map (function i, _ -> i) |> Iter.min

  let fills hole =
    Bitarray.to_list hole
    |> List.filter_mapi ~f:(fun i b -> if b then Some (`Pred (Fills_hole i)) else None)

  let lift = function
    | Value.Int _ -> Iter.empty
    | Value.Error -> Iter.singleton `False
    | Value.Matches { value = [ ms ]; holes } ->
        let p =
          longest_match ms
          |> Option.map ~f:(fun l -> [ `Pred (Max_match_is_at_most l) ])
          |> Option.value ~default:[ `Pred (Max_match_is_at_most (-1)) ]
        in
        let pf =
          first_match ms
          |> Option.map ~f:(fun l -> [ `Pred (First_match_is_after l) ])
          |> Option.value ~default:[ `Pred (First_match_is_after Int.max_value) ]
        in
        Iter.of_list (p @ pf @ fills holes)
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
    | `Pred (First_match_is_after x) ->
        List.range x 30 |> List.map ~f:(fun x -> `Pred (First_match_is_after x))
    | (`True | `False | `Pred (Fills_hole _)) as p -> [ p ]

  let implies ps ps' =
    Set.is_subset
      (Set.of_list (module P) @@ List.concat_map ~f:complete ps')
      ~of_:(Set.of_list (module P) @@ List.concat_map ~f:complete ps)

  let eval p c =
    match (p, c) with
    | Max_match_is_at_most k, Value.Matches { value = [ ms ]; _ } ->
        all_matches ms |> Iter.for_all (function i, j -> j - i <= k)
    | First_match_is_after k, Value.Matches { value = [ ms ]; _ } ->
        all_matches ms |> Iter.for_all (function i, _ -> k <= i)
    | Fills_hole i, (Value.Matches { holes; _ } | Value.Int { holes; _ }) ->
        Bitarray.get holes i
    | (Fills_hole _ | Max_match_is_at_most _ | First_match_is_after _), _ ->
        failwith "unexpected value"

  let transfer_not = function `Pred (Fills_hole _) as p -> [ p ] | _ -> []

  let transfer_optional = function
    | `True -> [ `Pred (First_match_is_after 0) ]
    | `Pred (Fills_hole _) as p -> [ p ]
    | _ -> []

  let transfer_and input a b =
    match (a, b) with
    | `Pred (Max_match_is_at_most m), `Pred (Max_match_is_at_most m') ->
        [ `Pred (Max_match_is_at_most (min m m')) ]
    | `Pred (First_match_is_after k), `Pred (First_match_is_after k') ->
        [ `Pred (First_match_is_after (max k k')) ]
    | `True, `Pred (First_match_is_after k) | `Pred (First_match_is_after k), `True ->
        [ `Pred (Max_match_is_at_most (String.length input - k)) ]
    | `True, (`Pred (Fills_hole _) as p) | (`Pred (Fills_hole _) as p), `True -> [ p ]
    | _ -> []

  let transfer_or a b =
    match (a, b) with
    | `Pred (Max_match_is_at_most m), `Pred (Max_match_is_at_most m') ->
        [ `Pred (Max_match_is_at_most (max m m')) ]
    | `Pred (Max_match_is_at_most m), `Concrete (Value.Matches { value = [ ms ]; _ })
    | `Concrete (Value.Matches { value = [ ms ]; _ }), `Pred (Max_match_is_at_most m) ->
        [
          `Pred
            (Max_match_is_at_most (max m (Option.value ~default:(-1) @@ longest_match ms)));
        ]
    | `Pred (First_match_is_after k), `Pred (First_match_is_after k') ->
        [ `Pred (First_match_is_after (min k k')) ]
    | `Pred (First_match_is_after k), `Concrete (Value.Matches { value = [ ms ]; _ })
    | `Concrete (Value.Matches { value = [ ms ]; _ }), `Pred (First_match_is_after k) ->
        [
          `Pred
            (First_match_is_after
               (min k (Option.value ~default:Int.max_value @@ first_match ms)));
        ]
    | `True, (`Pred (Fills_hole _) as p) | (`Pred (Fills_hole _) as p), `True -> [ p ]
    | _ -> []

  let transfer_concat input a b =
    match (a, b) with
    | `Pred (Max_match_is_at_most m), `Pred (Max_match_is_at_most m') ->
        [ `Pred (Max_match_is_at_most (min (String.length input) (m + m'))) ]
    | `Pred (Max_match_is_at_most m), `Concrete (Value.Matches { value = [ ms ]; _ })
    | `Concrete (Value.Matches { value = [ ms ]; _ }), `Pred (Max_match_is_at_most m) -> (
        match longest_match ms with
        | Some m' -> [ `Pred (Max_match_is_at_most (min (String.length input) (m + m'))) ]
        | None -> [ `Pred (Max_match_is_at_most 0) ])
    | (`Pred (First_match_is_after _) as p), `True -> [ p ]
    | `True, (`Pred (Fills_hole _) as p) | (`Pred (Fills_hole _) as p), `True -> [ p ]
    | _ -> []

  let transfer_repeat input a b =
    match (a, b) with
    | `Pred (Max_match_is_at_most x), `Concrete (Value.Int i) ->
        [ `Pred (Max_match_is_at_most (min (String.length input) (x * i.value))) ]
    | (`Pred (First_match_is_after _) as p), `True -> [ p ]
    | `True, (`Pred (Fills_hole _) as p) | (`Pred (Fills_hole _) as p), `True -> [ p ]
    | _ -> []

  let transfer_repeat_at_least a b =
    match (a, b) with
    | (`Pred (First_match_is_after _) as p), `True -> [ p ]
    | `True, (`Pred (Fills_hole _) as p) | (`Pred (Fills_hole _) as p), `True -> [ p ]
    | _ -> []

  let transfer_repeat_range input a b c =
    match (a, b, c) with
    | `Pred (Max_match_is_at_most x), _, `Concrete (Value.Int i) ->
        [ `Pred (Max_match_is_at_most (min (String.length input) (x * i.value))) ]
    | _, `Concrete (Value.Int i), `Concrete (Value.Int j) when i.value > j.value ->
        [ `False ]
    | (`Pred (First_match_is_after _) as p), `True, `True -> [ p ]
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
        | Optional, [ a ] -> transfer_optional a
        | And, [ a; b ] -> transfer_and input a b
        | Or, [ a; b ] -> transfer_or a b
        | Concat, [ a; b ] -> transfer_concat input a b
        | Repeat, [ a; b ] -> transfer_repeat input a b
        | Repeat_at_least, [ a; b ] -> transfer_repeat_at_least a b
        | Repeat_range, [ a; b; c ] -> transfer_repeat_range input a b c
        | Sketch sk, args ->
            if
              List.for_all2_exn sk.arg_types args ~f:(fun (_, holes) p ->
                  match p with `Pred (Fills_hole i) -> Bitarray.get holes i | _ -> false)
            then fills sk.ret_hole_mask
            else
              let value_preds =
                Set.to_list
                @@ transfer_sketch preds ctx example sk
                @@ List.map ~f:(Set.singleton (module P)) args
              in
              value_preds
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

module Stats = struct
  type t = { runtime : Timer.t } [@@deriving yojson_of]

  let create () = { runtime = Timer.create () }
end

let stats = Stats.create ()

let write_output file m_prog =
  let program_size =
    Option.map m_prog ~f:(fun p -> Float.of_int @@ Program.size p)
    |> Option.value ~default:Float.nan
  in
  let program_json =
    Option.map m_prog ~f:(fun p -> `String (Sexp.to_string @@ Regex.serialize p))
    |> Option.value ~default:`Null
  in
  let open Yojson in
  let json =
    `Assoc
      [
        ("method", `String "syngar");
        ("program", program_json);
        ("program_size", `Float program_size);
        ("stats", [%yojson_of: Stats.t] stats);
      ]
  in
  Out_channel.with_file file ~f:(fun ch -> Safe.to_channel ch json)

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Solve regex problems with abstraction guided synthesis."
    [%map_open
      let sketch = flag "-sketch" (required string) ~doc:" regex sketch"
      and out = flag "-out" (required string) ~doc:" result file" in
      fun () ->
        let ctx, ops = Regex_bench.load_sketch_bench sketch In_channel.stdin in
        let check_abs_example (lhs, rhs) abs =
          Set.mem abs (`Pred (Abs_value.Fills_hole 0))
          && Set.for_all abs ~f:(function
               | `True -> true
               | `False -> false
               | `Concrete (Value.Matches { value = [ ms ] }) ->
                   Bool.(Bitarray.Blocked_matrix.get ms 0 (String.length lhs) = rhs)
               | `Concrete (Value.Matches { value = _ }) ->
                   failwith "unexpected number of matches"
               | `Concrete Value.Error -> false
               | `Concrete (Value.Int _) -> failwith "wrong type of value"
               | `Pred (Abs_value.Max_match_is_at_most k) ->
                   if rhs && String.length lhs > k then false else true
               | `Pred (Abs_value.First_match_is_after k) -> if rhs then k = 0 else k > 0
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
        Timer.start stats.runtime;
        write_output out None;
        let p =
          Synth.synth
            ~initial_preds:
              (List.init ctx.n_holes ~f:(fun i -> `Pred (Abs_value.Fills_hole i)))
            check_abs_example check_example ctx examples
            (Op.default_operators 15 @ ops)
        in
        Timer.stop stats.runtime;
        write_output out p]
