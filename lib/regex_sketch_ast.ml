type unop = Not | Startwith | Endwith | Contain | Star [@@deriving sexp]
type binop = And | Or | Concat [@@deriving sexp]
type num = [ `Num of int | `Hole ] [@@deriving sexp]

type 't ast =
  [ `Class of string
  | `Unop of unop * 't
  | `Binop of binop * 't * 't
  | `Repeat of 't * num
  | `Repeat_at_least of 't * num
  | `Repeat_range of 't * num * num
  | `Hole of 't list ]
[@@deriving sexp]

type t = t ast [@@deriving sexp]

let number_holes sk =
  let id = ref 0 in
  let rec f = function
    | `Class x -> `Class x
    | `Unop (op, x) -> `Unop (op, f x)
    | `Binop (op, x, x') -> `Binop (op, f x, f x')
    | `Repeat (x, x') -> `Repeat (f x, f_hole x')
    | `Repeat_at_least (x, x') -> `Repeat_at_least (f x, f_hole x')
    | `Repeat_range (x, x', x'') -> `Repeat_range (f x, f_hole x', f_hole x'')
    | `Hole xs ->
        let xs = List.map xs ~f in
        incr id;
        `Hole (xs, !id)
  and f_hole = function
    | `Num x -> `Num x
    | `Hole ->
        incr id;
        `Hole !id
  in
  (f sk, !id)

let reduce reduce zero ( + ) = function
  | `Class _ | `Hole _ -> zero
  | `Unop (_, x) | `Repeat (x, _) | `Repeat_at_least (x, _) | `Repeat_range (x, _, _) ->
      reduce x
  | `Binop (_, x, x') -> reduce x + reduce x'

let rec has_hole = function `Hole _ -> true | x -> reduce has_hole false ( || ) x

module P = Program
open Regex

let op (x : Op.t) = `Op x
let int x = P.Apply (op (Op.Int x), [])
let concat x x' = P.Apply (op Op.Concat, [ x; x' ])
let repeat x n = P.Apply (op Op.Repeat_range, [ x; n ])
let repeat_range x l h = P.Apply (op Op.Repeat_range, [ x; l; h ])
let not x = P.Apply (op Not, [ x ])
let ( || ) x x' = P.apply (op Or) ~args:[ x; x' ]
let ( && ) x x' = P.apply (op And) ~args:[ x; x' ]
let empty = P.apply (op Op.Empty)
let anything = empty || not empty

let desugar x =
  let sketches = ref [] in
  let rec desugar = function
    | `Class c -> P.apply (op (Op.class_of_string_exn c))
    | `Unop (Not, x) -> not (desugar x)
    | `Unop (Startwith, x) -> concat (desugar x) anything
    | `Unop (Endwith, x) -> concat anything (desugar x)
    | `Unop (Contain, x) -> concat anything (concat (desugar x) anything)
    | `Unop (Star, x) -> empty || repeat_range (desugar x) (int 1) (int 15)
    | `Binop (And, x, x') -> desugar x && desugar x'
    | `Binop (Or, x, x') -> desugar x || desugar x'
    | `Binop (Concat, x, x') -> concat (desugar x) (desugar x')
    | `Repeat (x, n) -> repeat (desugar x) (desugar_num n)
    | `Repeat_at_least (x, n) -> repeat_range (desugar x) (desugar_num n) (int 15)
    | `Repeat_range (x, n, n') ->
        repeat_range (desugar x) (desugar_num n) (desugar_num n')
    | `Hole (xs, n) ->
        let xs' =
          Iter.of_list xs |> Iter.map desugar
          |> Iter.map (fun sk -> (sk, n))
          |> Iter.to_list
        in
        sketches := xs' @ !sketches;
        Apply (`Hole (n, Type.Regex), [])
  and desugar_num = function
    | `Num x -> P.apply (op (Op.Int x))
    | `Hole n -> Apply (`Hole (n, Type.Int), [])
  in
  let x' = desugar x in
  (x', 0) :: !sketches

let rec n_holes = function P.Apply (`Hole _, _) -> 1 | p -> P.reduce n_holes 0 ( + ) p

let renumber_holes x n =
  let id = ref 0 in
  let n_holes = n_holes x in
  let holes = ref [] in
  let rec f = function
    | P.Apply (`Op op, args) -> P.Apply (Op.Op op, List.map ~f args)
    | P.Apply (`Hole (n, t), []) ->
        let ret = P.Apply (Op.Hole !id, []) in
        incr id;
        holes := !holes @ [ (t, Bitarray.one_hot ~len:n_holes n) ];
        ret
    | P.Apply (`Hole _, _ :: _) -> failwith "hole with arguments"
  in
  let sketch = f x in
  Op.
    {
      term = sketch;
      arg_types = !holes;
      ret_type = Regex;
      ret_hole_mask = Bitarray.one_hot ~len:n_holes n;
    }

let convert_sketch sk =
  let sk, n_holes = number_holes sk in
  let sks = desugar sk in
  (List.map sks ~f:(fun (sk, n) -> renumber_holes sk n), n_holes)
