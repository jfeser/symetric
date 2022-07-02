open Std
module P = Program

module Type = struct
  type t = Int | Regex | Output [@@deriving compare, equal, hash, sexp, yojson]

  let default = Int
  let output = Output
  let pp fmt x = Sexp.pp fmt @@ [%sexp_of: t] x
end

module Op = struct
  type bitarray = Bitarray.t [@@deriving compare, equal, hash, sexp]

  let yojson_of_bitarray b = [%yojson_of: bool list] @@ Bitarray.to_list b
  let bitarray_of_yojson _ = assert false

  type multi_class = { name : string; mem : char -> bool [@ignore] [@opaque] }
  [@@deriving compare, equal, hash, sexp, yojson]

  type char_class = Single of char | Multi of multi_class
  [@@deriving compare, equal, hash, sexp, yojson]

  type sketch_op = Op of t | Hole of int

  and sketch = {
    id : int;
    term : sketch_op P.t; [@ignore]
    arg_types : (Type.t * bitarray) list; [@ignore]
    ret_type : Type.t; [@ignore]
    ret_hole_mask : bitarray; [@ignore]
  }

  and t =
    | Sketch of sketch
    | Int of int
    | Concat
    | Class of char_class
    | Repeat
    | Repeat_range
    | Repeat_at_least
    | Optional
    | Or
    | And
    | Not
    | Empty
  [@@deriving compare, equal, hash, sexp, yojson]

  let default = Concat
  let cost _ = 1

  let ret_type : _ -> Type.t = function
    | Concat | Class _ | Repeat_range | Repeat | Repeat_at_least | Optional | Or | And
    | Not | Empty ->
        Regex
    | Int _ -> Int
    | Sketch x -> x.ret_type

  let args_type : _ -> Type.t list = function
    | Class _ | Int _ | Empty -> []
    | Optional | Not -> [ Regex ]
    | Concat | Or | And -> [ Regex; Regex ]
    | Repeat | Repeat_at_least -> [ Regex; Int ]
    | Repeat_range -> [ Regex; Int; Int ]
    | Sketch x -> List.map ~f:Tuple.T2.get1 x.arg_types

  let arity op = List.length @@ args_type op
  let is_commutative = function Or | And -> true | _ -> false
  let pp fmt x = Sexp.pp_hum fmt @@ [%sexp_of: t] x

  open Program.T

  let class_ c = Class (Single c)

  let alpha =
    { name = "let"; mem = (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false) }

  let num = { name = "num"; mem = (function '0' .. '9' -> true | _ -> false) }
  let num19 = { name = "num1-9"; mem = (function '1' .. '9' -> true | _ -> false) }
  let cap = { name = "cap"; mem = (function 'A' .. 'Z' -> true | _ -> false) }
  let low = { name = "low"; mem = (function 'a' .. 'z' -> true | _ -> false) }

  let spec =
    {
      name = "spec";
      mem =
        (function
        | '!' .. '/' | ':' .. '@' | '[' .. '`' | '{' .. '~' -> true | _ -> false);
    }

  let alphanum =
    {
      name = "alphanum";
      mem = (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false);
    }

  let hex =
    {
      name = "hex";
      mem = (function 'a' .. 'f' | 'A' .. 'F' | '0' .. '9' -> true | _ -> false);
    }

  let vow =
    {
      name = "vow";
      mem =
        (function
        | 'a' | 'e' | 'i' | 'o' | 'u' | 'A' | 'E' | 'I' | 'O' | 'U' -> true | _ -> false);
    }

  let any = { name = "any"; mem = (function ' ' .. '~' -> true | _ -> false) }
  let named_classes = [ alpha; num; num19; cap; low; any; spec; alphanum; hex ]
  let alpha = Class (Multi alpha)
  let num = Class (Multi num)
  let cap = Class (Multi cap)
  let low = Class (Multi low)
  let any = Class (Multi any)

  let class_of_string_exn s =
    if String.length s = 1 then Class (Single s.[0])
    else
      match List.find named_classes ~f:(fun c -> [%equal: string] c.name s) with
      | Some c -> Class (Multi c)
      | None -> raise_s [%message "unexpected class name" s]

  let int x = Apply (Int x, [])
  let concat x x' = Apply (Concat, [ x; x' ])
  let repeat x n = Apply (Repeat, [ x; int n ])
  let repeat_range x l h = Apply (Repeat_range, [ x; int l; int h ])
  let repeat_at_least x n = Apply (Repeat_at_least, [ x; int n ])
  let optional x = Apply (Optional, [ x ])
  let not x = Apply (Not, [ x ])
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

  type 'a hole_constr = { value : 'a; holes : Bitarray.t }
  [@@deriving compare, equal, hash, sexp]

  type t = Int of int hole_constr | Matches of match_ list hole_constr | Error
  [@@deriving compare, equal, hash, sexp]

  let default = Error

  module Ctx = struct
    type t = { input : (string * bool) list; max_example_len : int; n_holes : int }
    [@@deriving sexp]

    let create input n_holes =
      {
        input;
        max_example_len =
          List.map input ~f:(fun (s, _) -> String.length s) |> List.fold ~init:0 ~f:max;
        n_holes;
      }
  end

  let eval_unmemoized_open eval (ctx : Ctx.t) (op : Op.t) args =
    let fail () = raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)] in
    let wrap0 x = { value = x; holes = Bitarray.create ctx.n_holes false } in
    let wrap2 f x x' =
      { value = f x.value x'.value; holes = Bitarray.O.(x.holes lor x'.holes) }
    in
    match (op, args) with
    | Int x, [] -> Int (wrap0 x)
    | Empty, [] ->
        Matches
          (wrap0
          @@ List.map ctx.input ~f:(fun (input, _) ->
                 M.identity (String.length input + 1)))
    | Class (Single c), [] ->
        eval ctx
          (Op.Class
             (Multi { mem = (fun c' -> [%equal: Char.t] c c'); name = Char.to_string c }))
          []
    | Class (Multi { mem; _ }), [] ->
        Matches
          (wrap0
          @@ List.map ctx.input ~f:(fun (input, _) ->
                 Iter.int_range ~start:0 ~stop:(String.length input - 1)
                 |> Iter.filter (fun idx -> mem input.[idx])
                 |> Iter.fold
                      (fun m i -> M.set m i (i + 1) true)
                      (M.create (String.length input + 1) false)))
    | (Int _ | Empty | Class _), _ :: _ -> fail ()
    | (Not | Optional), [ Error ] -> Error
    | Not, [ Matches x ] ->
        Matches
          {
            x with
            value =
              List.map x.value ~f:(fun m -> M.(O.(lnot m land upper_triangle (dim m))));
          }
    | (Not | Optional), ([] | [ Int _ ] | _ :: _ :: _) -> fail ()
    | Optional, [ v ] -> eval ctx Or [ eval ctx Empty []; v ]
    | (And | Or | Concat | Repeat | Repeat_at_least), ([ Error; _ ] | [ _; Error ]) ->
        Error
    | (And | Or | Concat), ([] | [ _ ] | [ Int _; _ ] | [ _; Int _ ] | _ :: _ :: _ :: _)
      ->
        fail ()
    | Or, [ Matches x; Matches x' ] ->
        Matches (wrap2 (fun x x' -> List.map2_exn x x' ~f:M.O.( lor )) x x')
    | And, [ Matches x; Matches x' ] ->
        Matches (wrap2 (fun x x' -> List.map2_exn x x' ~f:M.O.( land )) x x')
    | Concat, [ Matches x; Matches x' ] ->
        Matches
          (wrap2 (fun x x' -> List.map2_exn x x' ~f:(fun x x' -> M.O.(x * x'))) x x')
    | ( (Repeat | Repeat_at_least),
        ([] | [ _ ] | [ Int _; _ ] | [ _; Matches _ ] | _ :: _ :: _ :: _) ) ->
        fail ()
    | Repeat, [ Matches ms; Int n ] ->
        Matches { ms with value = List.map ms.value ~f:(fun m -> M.pow m n.value) }
    | Repeat_at_least, [ Matches _; Int n ] when n.value > ctx.max_example_len ->
        eval ctx Empty []
    | Repeat_at_least, [ ms; n ] ->
        eval ctx Repeat_range [ ms; n; Int (wrap0 ctx.max_example_len) ]
    | Repeat_range, ([ Error; _; _ ] | [ _; Error; _ ] | [ _; _; Error ]) -> Error
    | Repeat_range, [ _; Int min; Int max ] when max.value < min.value -> Error
    | Repeat_range, [ Matches ms; Int min; Int max ] ->
        Matches
          {
            ms with
            value =
              List.map ms.value ~f:(fun m -> M.transitive_range m min.value max.value);
          }
    | ( Repeat_range,
        ( []
        | [ _ ]
        | [ _; _ ]
        | [ Int _; _; _ ]
        | [ _; Matches _; _ ]
        | [ _; _; Matches _ ]
        | _ :: _ :: _ :: _ :: _ ) ) ->
        fail ()
    | Sketch sk, args -> (
        let args_valid =
          List.fold2_exn sk.arg_types args ~init:true ~f:(fun rest_valid (_, mask) arg ->
              let check_holes holes = Bitarray.(any O.(mask land holes)) in
              match arg with
              | Error -> false
              | Int x -> rest_valid && check_holes x.holes
              | Matches x -> rest_valid && check_holes x.holes)
        in
        if not args_valid then Error
        else
          let eval_sketch op xs =
            match op with Op.Op op -> eval ctx op xs | Hole i -> List.nth_exn args i
          in
          let ret = P.eval eval_sketch sk.term in
          match ret with
          | Error -> ret
          | Int x -> Int { x with holes = sk.ret_hole_mask }
          | Matches x -> Matches { x with holes = sk.ret_hole_mask })

  let rec eval_unmemoized ctx op args = eval_unmemoized_open eval_unmemoized ctx op args

  let mk_eval_memoized () =
    let module Key = struct
      module T = struct
        type nonrec t = Op.t * t list [@@deriving compare, hash, sexp]
      end

      include T
      include Comparable.Make (T)
    end in
    let tbl = Hashtbl.create (module Key) in
    let rec find_or_eval (ctx : Ctx.t) op args =
      match Hashtbl.find tbl (op, args) with
      | Some v -> v
      | None ->
          let v = eval_unmemoized_open find_or_eval ctx op args in
          Hashtbl.set tbl ~key:(op, args) ~data:v;
          v
    in
    find_or_eval

  let eval = eval_unmemoized

  let target_distance (ctx : Ctx.t) = function
    | Matches m when Bitarray.length m.holes = 0 || Bitarray.get m.holes 0 ->
        let correct =
          List.fold2_exn ctx.input m.value ~init:0 ~f:(fun acc (s, is_pos) m ->
              let full_match = M.get m 0 (String.length s) in
              if [%equal: bool] is_pos full_match then acc + 1 else acc)
        in
        1. -. (Float.of_int correct /. Float.of_int (List.length ctx.input))
    | _ -> 1.

  let is_error = function Error -> true | _ -> false

  let distance v v' =
    match (v, v') with
    | Int x, Int x' -> if x.value = x'.value then 0. else 1.
    | Matches ms, Matches ms' when [%equal: Bitarray.t] ms.holes ms'.holes ->
        let distance =
          List.map2_exn ms.value ms'.value ~f:(fun m m' ->
              Bitarray.jaccard_distance (M.to_bitarray m) (M.to_bitarray m'))
          |> Iter.of_list |> Iter.mean |> Option.value ~default:0.
        in
        distance
    | _ -> 1.

  let pp fmt = function
    | Matches m ->
        Sexp.pp_hum fmt
          ([%sexp_of: bool list] (List.map m.value ~f:(fun m -> M.get m 0 (M.dim m - 1))))
    | _ -> ()
end

let serialize = [%sexp_of: Op.t Program.t]
let parse = [%of_sexp: Op.t Program.t]
