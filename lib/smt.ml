open! Core

let debug_out_file =
  let ctr = ref 0 in
  fun () ->
    incr ctr;
    sprintf "interp%d.smt2" !ctr

let pp_sig fmt (name, n_args) =
  let name = String_id.to_string name in
  if n_args = 0 then Fmt.pf fmt "%s () Bool" name
  else if n_args = 1 then Fmt.pf fmt "%s (Bool) Bool" name
  else if n_args = 2 then Fmt.pf fmt "%s (Bool Bool) Bool" name
  else
    let args =
      List.init n_args ~f:(fun _ -> "Bool") |> String.concat ~sep:" "
    in
    Fmt.pf fmt "%s (%s) Bool" name args

module Var = String_id

module Model0 = struct end

module Expr = struct
  type binop = Implies | Equals [@@deriving compare, sexp]

  type unop = Not [@@deriving compare, sexp]

  type varop = And | Or [@@deriving compare, sexp]

  type t =
    | Bool of bool
    | Var of Var.t
    | Binop of binop * t * t
    | Unop of unop * t
    | Varop of varop * t list
    | Let of (t * Var.t * t)
    | Annot of t * string * string
  [@@deriving compare, variants, sexp]

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

  let var_s x = Var (String_id.of_string x)

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
    | Atom "true" -> return @@ Bool true
    | Atom "false" -> return @@ Bool false
    | Atom x -> return @@ var_s x
    | inter -> Or_error.error "Unexpected interpolant" inter [%sexp_of: Sexp.t]

  let rec eval ctx = function
    | Bool v -> v
    | Var x -> Map.find_exn ctx x
    | Annot (e, _, _) -> eval ctx e
    | Let (e, x, e') -> eval (Map.set ctx ~key:x ~data:(eval ctx e)) e'
    | Binop (op, e, e') -> (
        let v = eval ctx e and v' = eval ctx e' in
        match op with Implies -> (not v) || v' | Equals -> Bool.(v = v') )
    | Unop (op, e) -> (
        let v = eval ctx e in
        match (op, v) with Not, v -> not v )
    | Varop (op, es) -> (
        let vs = List.map es ~f:(eval ctx) in
        match op with
        | And ->
            List.sum
              ( module struct
                type t = bool

                let ( + ) = ( && )

                let zero = true
              end )
              ~f:Fun.id vs
        | Or ->
            List.sum
              ( module struct
                type t = bool

                let ( + ) = ( || )

                let zero = false
              end )
              ~f:Fun.id vs )

  let reduce reduce plus empty = function
    | Bool _ | Var _ -> empty
    | Binop (_, e, e') | Let (e, _, e') -> plus (reduce e) (reduce e')
    | Annot (e, _, _) | Unop (_, e) -> reduce e
    | Varop (_, es) ->
        List.fold ~init:empty ~f:(fun acc e -> plus (reduce e) acc) es

  let map map = function
    | (Bool _ | Var _) as e -> e
    | Binop (op, e, e') -> Binop (op, map e, map e')
    | Let (e, v, e') -> Let (map e, v, map e')
    | Annot (e, x, y) -> Annot (map e, x, y)
    | Unop (op, e) -> Unop (op, map e)
    | Varop (op, es) -> Varop (op, List.map ~f:map es)

  let rec vars =
    let open String_id in
    function
    | Var v -> Set.singleton v
    | Let (e, v, e') -> Set.remove (Set.union (vars e) (vars e')) v
    | e -> reduce vars Set.union Set.empty e

  let expr_vars = vars

  let rec expand ctx = function
    | Var x as e -> (
        match Map.find ctx x with Some e -> expand ctx e | None -> e )
    | e -> map (expand ctx) e
end

open Expr

let var_s = Expr.var_s

let var x = Var x

let false_ = Bool false

let true_ = Bool true

let is_false = function Bool x -> not x | _ -> false

let is_true = function Bool x -> x | _ -> false

let or_ xs =
  let or_simple = function [] -> false_ | [ x ] -> x | xs -> Varop (Or, xs) in
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

module Decl = struct
  type t = { name : Var.t; n_args : int }

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

module Assert = struct
  type t = { body : Expr.t; group : int option }

  let to_smtlib fmt { body; group } =
    match group with
    | Some gid ->
        Fmt.pf fmt "(assert (! %a :interpolation-group g%d))" Expr.pp body gid
    | None -> Fmt.pf fmt "(assert %a)" Expr.pp body
end

module Revlist : sig
  type 'a t

  val empty : 'a t

  val append : 'a t -> 'a -> 'a t

  val append_many : 'a t -> 'a list -> 'a t

  val to_list : 'a t -> 'a list

  val filter : 'a t -> f:('a -> bool) -> 'a t
end = struct
  type 'a t = 'a list

  let empty = []

  let append x l = l :: x

  let append_many xs xs' = List.fold_left xs' ~init:xs ~f:(fun xs x -> x :: xs)

  let to_list = List.rev

  let filter = List.filter
end

type stmt =
  | Decl of Decl.t
  | Defn of Defn.t
  | Assert of Assert.t
  | Comment of string
  | Newline

type state = {
  stmts : (stmt * string) Revlist.t;
  var_ctr : int;
  group_ctr : int;
}

type 'a t = state -> 'a * state

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let bind run ~f s =
    let x, s' = run s in
    f x s'

  let return x s = (x, s)

  let map = `Define_using_bind
end)

let with_state s f = f s

let run c = with_state { stmts = Revlist.empty; var_ctr = 0; group_ctr = 0 } c

let eval c = Tuple.T2.get1 @@ run c

let eval_with_state s c = Tuple.T2.get1 @@ with_state s c

let clear_asserts s =
  let stmts' =
    Revlist.filter s.stmts ~f:(fun (stmt, _) ->
        match stmt with Assert _ -> false | _ -> true)
  in
  ((), { s with stmts = stmts' })

open Let_syntax

let string_of_stmt = function
  | Decl d -> Fmt.str "%a" Decl.to_smtlib d
  | Defn d -> Fmt.str "%a" Defn.to_smtlib d
  | Assert a -> Fmt.str "%a" Assert.to_smtlib a
  | Comment c ->
      String.split_lines c
      |> List.map ~f:(Fmt.str "; %s")
      |> String.concat ~sep:"\n"
  | Newline -> ""

let add_stmt stmt =
  let stmt_str = string_of_stmt stmt in
  fun s -> ((), { s with stmts = Revlist.append s.stmts (stmt, stmt_str) })

let add_stmts stmts =
  let stmt_strs = List.map stmts ~f:(fun stmt -> (stmt, string_of_stmt stmt)) in
  fun s -> ((), { s with stmts = Revlist.append_many s.stmts stmt_strs })

let get_stmts s = (s.stmts, s)

let fresh_num s = (s.var_ctr, { s with var_ctr = s.var_ctr + 1 })

let make_decl ?n_args name =
  let name = String_id.of_string name in
  let%bind () = add_stmt @@ Decl (Decl.create ?n_args name) in
  return name

let fresh_decl ?n_args ?(prefix = "x") () =
  let%bind ctr = fresh_num in
  let name = sprintf "%s%d" prefix ctr in
  make_decl ?n_args name

let make_defn ?n_args name body =
  let name = String_id.of_string name in
  let%bind () = add_stmt @@ Defn (Defn.create ?n_args name body) in
  return name

let fresh_defn ?n_args ?(prefix = "x") body =
  let%bind ctr = fresh_num in
  let name = sprintf "%s%d" prefix ctr in
  make_defn ?n_args name body

let with_comment_block ~name ?descr body =
  let sep = String.make 80 '*' in
  let header =
    [ Newline; Comment sep; Comment (Fmt.str "Begin: %s" name); Comment sep ]
    @ ( Option.map descr ~f:(fun d ->
            [ Comment (Sexp.to_string_hum d); Comment sep ])
      |> Option.value ~default:[] )
    @ [ Newline ]
  in
  let footer =
    [
      Newline;
      Comment sep;
      Comment (Fmt.str "End: %s" name);
      Comment sep;
      Newline;
    ]
  in

  let%bind () = add_stmts header in
  let%bind ret = body in
  let%map () = add_stmts footer in
  ret

let annotate key value term = Annot (term, key, value)

let comment = annotate "comment"

let assert_ body = add_stmt @@ Assert { body; group = None }

module Interpolant = struct
  module Group = struct
    type t = int

    let create s = (s.group_ctr, { s with group_ctr = s.group_ctr + 1 })

    let pp fmt x = Fmt.pf fmt "g%d" x
  end

  let group_vars s =
    let stmts = s.stmts |> Revlist.to_list in
    let ctx =
      List.filter_map stmts ~f:(fun (stmt, _) ->
          match stmt with
          | Defn ({ name; _ }, body) -> Some (name, body)
          | _ -> None)
      |> Map.of_alist_exn (module String_id)
    in
    let k g =
      List.filter_map stmts ~f:(fun (stmt, _) ->
          match stmt with
          | Assert { group = Some gid; body } when Int.(g = gid) ->
              Some (expand ctx body |> Expr.vars)
          | _ -> None)
      |> Set.union_list (module Var)
    in
    (k, s)

  let assert_group ?group body =
    let%bind group =
      match group with Some g -> return g | None -> Group.create
    in
    add_stmt @@ Assert { body; group = Some group }
end

let read_input = Sexp.input_sexp

let with_mathsat f =
  let proc = Unix.open_process "mathsat" in
  let stdout, stdin = proc in
  let ret =
    let fn = debug_out_file () in
    Out_channel.with_file fn ~f:(fun log ->
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
  Unix.close_process proc |> Unix.Exit_or_signal.or_error |> Or_error.ok_exn;
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

let smtlib =
  let%map stmts = get_stmts in
  Revlist.to_list stmts
  |> List.map ~f:(fun (_, str) -> str)
  |> String.concat ~sep:"\n"

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
  let%bind stmts = get_stmts in
  with_mathsat
  @@ get_interpolant_or_model_inner groups
  @@ List.map ~f:Tuple.T2.get2 @@ Revlist.to_list stmts

let get_model =
  let open Sexp in
  let%bind stmts = get_stmts in
  let stmts = List.map ~f:Tuple.T2.get2 @@ Revlist.to_list stmts in
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
  let stmts = List.map ~f:Tuple.T2.get2 @@ Revlist.to_list stmts in
  with_mathsat @@ fun read write ->
  write (stmts @ [ "(check-sat)" ]);
  return
    ( match read () with
    | Atom "unsat" -> false
    | Atom "sat" -> true
    | x -> error x )

module Model = struct
  module T = struct
    module T0 = struct
      type t = bool Map.M(Var).t [@@deriving compare, sexp]
    end

    include T0
    include Comparator.Make (T0)
  end

  include T

  let of_ ?vars e =
    let vars = Option.map vars ~f:(Set.inter (Expr.vars e)) in
    print_s
      [%message "enumerating models" (vars : Set.M(Var).t option) (e : Expr.t)];
    let bit m i = Int.((m lsr i) land 0x1 > 0) in
    let open Sequence in
    let all_vars = expr_vars e |> Set.to_list in
    let post_filter =
      match vars with
      | Some vars ->
          fun models ->
            map models ~f:(Map.filter_keys ~f:(Set.mem vars))
            |> to_list
            |> Set.of_list (module T)
            |> Set.to_sequence
      | None -> Fun.id
    in
    range 0 (Int.pow 2 (List.length all_vars))
    |> filter_map ~f:(fun m ->
           let ctx =
             List.mapi all_vars ~f:(fun i v -> (v, bit m i))
             |> Map.of_alist_exn (module String_id)
           in
           if Expr.eval ctx e then Some ctx else None)
    |> post_filter

  let%expect_test "" =
    of_ (varop And [ var_s "x"; var_s "y" ])
    |> [%sexp_of: bool Map.M(Var).t Sequence.t] |> print_s;
    [%expect {| (((x true) (y true))) |}]

  let%expect_test "" =
    of_ (varop Or [ var_s "x"; var_s "y" ])
    |> [%sexp_of: bool Map.M(Var).t Sequence.t] |> print_s;
    [%expect
      {| (((x true) (y false)) ((x false) (y true)) ((x true) (y true))) |}]

  let to_expr m =
    Map.to_alist m |> List.map ~f:(fun (k, v) -> var k = bool v) |> and_
end
