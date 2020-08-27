open! Core

[@@@landmark "auto"]

let debug_out_file =
  let ctr = ref 0 in
  fun () ->
    incr ctr;
    sprintf "interp%d.smt2" !ctr

let pp_sig fmt (name, n_args) =
  if n_args = 0 then Fmt.pf fmt "%s () Bool" name
  else if n_args = 1 then Fmt.pf fmt "%s (Bool) Bool" name
  else if n_args = 2 then Fmt.pf fmt "%s (Bool Bool) Bool" name
  else
    let args =
      List.init n_args ~f:(fun _ -> "Bool") |> String.concat ~sep:" "
    in
    Fmt.pf fmt "%s (%s) Bool" name args

module Expr = struct
  type binop = Implies | Equals [@@deriving equal, sexp]

  type unop = Not [@@deriving equal, sexp]

  type varop = And | Or [@@deriving equal, sexp]

  type var = String_id.t [@@deriving equal, sexp]

  type t =
    | Bool of bool
    | Var of var
    | Binop of binop * t * t
    | Unop of unop * t
    | Varop of varop * t list
    | Let of (t * var * t)
    | Annot of t * string * string
  [@@deriving equal, sexp]

  let pp_binop fmt = function
    | Implies -> Fmt.pf fmt "=>"
    | Equals -> Fmt.pf fmt "="

  let pp_unop fmt = function Not -> Fmt.pf fmt "not"

  let pp_varop fmt = function And -> Fmt.pf fmt "and" | Or -> Fmt.pf fmt "or"

  let rec pp fmt = function
    | Bool true -> Fmt.pf fmt "true"
    | Bool false -> Fmt.pf fmt "false"
    | Binop (op, x, x') -> Fmt.pf fmt "(%a %a %a)" pp_binop op pp x pp x'
    | Unop (op, x) -> Fmt.pf fmt "(%a %a)" pp_unop op pp x
    | Varop (op, xs) ->
        Fmt.pf fmt "(%a %a)" pp_varop op Fmt.(list ~sep:sp pp) xs
    | Annot (x, k, v) -> Fmt.pf fmt "(! %a :%s %s)" pp x k v
    | Var v -> String_id.pp fmt v
    | Let (e, v, e') ->
        Fmt.pf fmt "(let ((%a %a)) %a)" String_id.pp v pp e pp e'

  let var x = Var (String_id.of_string x)

  let rec parse =
    let open Or_error.Let_syntax in
    function
    | Sexp.List (Atom "or" :: args) ->
        let%map args = List.map ~f:parse args |> Or_error.all in
        Varop (Or, args)
    | Sexp.List (Atom "and" :: args) ->
        let%map args = List.map ~f:parse args |> Or_error.all in
        Varop (And, args)
    | Sexp.List [ Atom "not"; arg ] ->
        let%map arg = parse arg in
        Unop (Not, arg)
    | List [ Atom "let"; List [ List [ Atom var; lhs ] ]; rhs ] ->
        let%bind lhs = parse lhs in
        let%bind rhs = parse rhs in
        return @@ Let (lhs, String_id.of_string var, rhs)
    | Atom x -> return @@ var x
    | inter -> Or_error.error "Unexpected interpolant" inter [%sexp_of: Sexp.t]

  let reduce reduce plus empty = function
    | Bool _ | Var _ -> empty
    | Binop (_, e, e') | Let (e, _, e') -> plus (reduce e) (reduce e')
    | Annot (e, _, _) | Unop (_, e) -> reduce e
    | Varop (_, es) ->
        List.fold ~init:empty ~f:(fun acc e -> plus (reduce e) acc) es

  let rec vars =
    let open String_id in
    function
    | Var v -> Set.singleton v
    | Let (e, v, e') -> Set.remove (Set.union (vars e) (vars e')) v
    | e -> reduce vars Set.union Set.empty e
end

open Expr

module Decl = struct
  type t = { name : string; n_args : int }

  let create ?(n_args = 0) name = { name; n_args }

  let to_smtlib fmt { name; n_args } =
    Fmt.pf fmt "(declare-fun %a)" pp_sig (name, n_args)
end

module Defn = struct
  type t = Decl.t * Expr.t

  let create ?n_args name body = (Decl.create ?n_args name, body)

  let to_smtlib fmt (Decl.{ name; n_args }, body) =
    Fmt.pf fmt "(define-fun %a %a)" pp_sig (name, n_args) Expr.pp body
end

type stmt = Decl of Decl.t | Defn of Defn.t | Assert of Expr.t

type state = { stmts : string list; var_ctr : int; group_ctr : int }

type 'a t = State of (state -> 'a * state) [@@unboxed]

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let bind (State run) ~f =
    State
      (fun s ->
        let x, s' = run s in
        let (State run') = f x in
        run' s')

  let return x = State (fun s -> (x, s))

  let map = `Define_using_bind
end)

let with_state s (State f) = f s

let run c = with_state { stmts = []; var_ctr = 0; group_ctr = 0 } c

let eval c = Tuple.T2.get1 @@ run c

let eval_with_state s c = Tuple.T2.get1 @@ with_state s c

open Let_syntax

let add_stmt stmt = State (fun s -> ((), { s with stmts = s.stmts @ [ stmt ] }))

let get_stmts = State (fun s -> (s.stmts, s))

let fresh_num = State (fun s -> (s.var_ctr, { s with var_ctr = s.var_ctr + 1 }))

let make_decl ?n_args name =
  let%bind () =
    add_stmt (Fmt.str "%a" Decl.to_smtlib @@ Decl.create ?n_args name)
  in
  return (String_id.of_string name)

let fresh_decl ?n_args ?(prefix = "x") () =
  let%bind ctr = fresh_num in
  let name = sprintf "%s%d" prefix ctr in
  make_decl ?n_args name

let make_defn ?n_args ?ret name body =
  let%bind () =
    add_stmt (Fmt.str "%a" Defn.to_smtlib @@ Defn.create ?n_args name body)
  in
  return (String_id.of_string name)

let fresh_defn ?n_args ?(prefix = "x") body =
  let%bind ctr = fresh_num in
  let name = sprintf "%s%d" prefix ctr in
  make_defn ?n_args name body

let annotate key value term = Annot (term, key, value)

let comment = annotate "comment"

module Bool = struct
  let false_ = Bool false

  let true_ = Bool true

  let is_false = function Bool x -> not x | _ -> false

  let is_true = function Bool x -> x | _ -> false

  let or_ xs =
    let or_simple = function
      | [] -> false_
      | [ x ] -> x
      | xs -> Varop (Or, xs)
    in
    let rec or_ acc = function
      | [] -> or_simple (List.rev acc)
      | Bool false :: xs -> or_ acc xs
      | Bool true :: _ -> true_
      | x :: xs -> or_ (x :: acc) xs
    in
    or_ [] xs

  let and_ xs =
    let and_simple = function
      | [] -> true_
      | [ x ] -> x
      | xs -> Varop (And, xs)
    in
    let rec and_ acc = function
      | [] -> and_simple (List.rev acc)
      | Bool false :: _ -> false_
      | Bool true :: xs -> and_ acc xs
      | x :: xs -> and_ (x :: acc) xs
    in
    and_ [] xs

  let not_ = function
    | Bool true -> false_
    | Bool false -> true_
    | x -> Unop (Not, x)

  let implies x y =
    if is_true x then y
    else if is_false x || is_true y then true_
    else if is_false y then not_ x
    else Binop (Implies, x, y)

  let ( = ) x y =
    match (x, y) with
    | Bool true, _ -> y
    | _, Bool true -> x
    | Bool false, _ -> not_ y
    | _, Bool false -> not_ x
    | _, _ -> Binop (Equals, x, y)

  let ( || ) x x' = or_ [ x; x' ]

  let ( && ) x x' = and_ [ x; x' ]

  let not = not_

  let ( => ) = implies

  let at_least_one = or_

  let at_most_one xs =
    let module Seq = Sequence in
    let xs = Array.of_list xs and n = List.length xs in
    Seq.init n ~f:(fun i ->
        Seq.range (i + 1) n |> Seq.map ~f:(fun j -> (not xs.(i)) || not xs.(j)))
    |> Seq.concat |> Seq.to_list |> and_

  let exactly_one xs = at_least_one xs && at_most_one xs

  let bool x = if x then true_ else false_
end

let assert_ expr = add_stmt (Fmt.str "(assert %a)" Expr.pp expr)

module Interpolant = struct
  module Group : sig
    type t

    type 'a s

    val create : t s

    val sexp_of : t -> Sexp.t

    val pp : t Fmt.t
  end
  with type 'a s := 'a t = struct
    type t = int

    let create =
      State (fun s -> (s.group_ctr, { s with group_ctr = s.group_ctr + 1 }))

    let sexp_of x = Sexp.Atom (Fmt.str "g%d" x)

    let pp fmt x = Fmt.pf fmt "g%d" x
  end

  let assert_group ?group expr =
    let%bind group =
      match group with Some g -> return g | None -> Group.create
    in
    assert_ @@ annotate "interpolation-group" (Fmt.str "%a" Group.pp group) expr
end

let read_input = Sexp.input_sexp

let with_mathsat f =
  let open Sexp in
  let proc = Unix.open_process "mathsat" in
  let stdout, stdin = proc in
  let ret =
    Out_channel.with_file (debug_out_file ()) ~f:(fun log ->
        let log_fmt = Format.formatter_of_out_channel log in

        let[@landmark "with_mathsat.write"] write stmts =
          Out_channel.output_lines stdin stmts;
          Out_channel.output_lines log stmts;
          Out_channel.flush stdin
        in

        let[@landmark "with_mathsat.read"] read () =
          let sexp = (read_input stdout [@landmark "mathsat"]) in
          Sexp.to_string_hum sexp |> String.split_lines
          |> List.iter ~f:(Fmt.pf log_fmt "; %s@.");
          sexp
        in

        f read write)
  in
  Unix.close_process proc |> ignore;
  ret

let error sexp =
  Error.create "Unexpected output" sexp [%sexp_of: Sexp.t] |> Error.raise

let parse_model sexp =
  let error s =
    Error.create "Unexpected model" s [%sexp_of: Sexp.t] |> Error.raise
  in
  let parse_value = function
    | Sexp.Atom "true" -> true
    | Atom "false" -> false
    | s -> error s
  in
  match sexp with
  | Sexp.List vals ->
      List.map vals ~f:(fun v ->
          match v with
          | List [ Atom name; value ] ->
              (String_id.of_string name, parse_value value)
          | s -> error s)
  | s -> error s

let get_interpolant_or_model_inner groups stmts read write =
  let open Sexp in
  write
    ( [
        "(set-option :produce-interpolants true)";
        "(set-option :produce-models true)";
      ]
    @ stmts @ [ "(check-sat)" ] );
  let is_sat =
    match read () with
    | Atom "unsat" -> false
    | Atom "sat" -> true
    | x -> error x
  in

  if is_sat then (
    write [ "(get-model)" ];
    return (Second (read () |> parse_model)) )
  else (
    write
      [
        Fmt.str "(get-interpolant (%a))"
          Fmt.(list ~sep:sp Interpolant.Group.pp)
          groups;
      ];
    return (First (read () |> Expr.parse)) )

let get_interpolant_or_model groups =
  let open Sexp in
  let%bind stmts = get_stmts in
  with_mathsat @@ get_interpolant_or_model_inner groups stmts

let get_model =
  let open Sexp in
  let%bind stmts = get_stmts in
  with_mathsat @@ fun read write ->
  write ([ "(set-option :produce-models true)" ] @ stmts @ [ "(check-sat)" ]);
  let is_sat =
    match read () with
    | Atom "unsat" -> false
    | Atom "sat" -> true
    | x -> error x
  in

  if is_sat then (
    write [ "(get-model)" ];
    return (read () |> parse_model |> Option.return) )
  else return None

let check_sat =
  let open Sexp in
  let%bind stmts = get_stmts in
  with_mathsat @@ fun read write ->
  write (stmts @ [ "(check-sat)" ]);
  return
    ( match read () with
    | Atom "unsat" -> false
    | Atom "sat" -> true
    | x -> error x )
