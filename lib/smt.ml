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

module Decl = struct
  type t = { name : string; n_args : int }

  let create ?(n_args = 0) name = { name; n_args }

  let to_smtlib fmt { name; n_args } =
    Fmt.pf fmt "(declare-fun %a)" pp_sig (name, n_args)
end

module Defn = struct
  type t = Decl.t * Sexp.t

  let create ?n_args name body = (Decl.create ?n_args name, body)

  let to_smtlib fmt (Decl.{ name; n_args }, body) =
    Fmt.pf fmt "(define-fun %a %a)" pp_sig (name, n_args) Sexp.pp body
end

type stmt =
  | Decl of Decl.t
  | Defn of Defn.t
  | Assert of Sexp.t
  | Extra of Sexp.t

type state = { stmts : stmt list; var_ctr : int; group_ctr : int }

type 'a t = State of (state -> 'a * state) [@@unboxed]

let make f = State f

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let bind (State run) ~f =
    make @@ fun s ->
    let x, s' = run s in
    let (State run') = f x in
    run' s'

  let return x = make @@ fun s -> (x, s)

  let map = `Define_using_bind
end)

let with_state s (State f) = f s

let run c = with_state { stmts = []; var_ctr = 0; group_ctr = 0 } c

let eval c = Tuple.T2.get1 @@ run c

let eval_with_state s c = Tuple.T2.get1 @@ with_state s c

open Let_syntax

let add_stmt stmt = make @@ fun s -> ((), { s with stmts = s.stmts @ [ stmt ] })

let get_stmts = make @@ fun s -> (s.stmts, s)

let fresh_num = make @@ fun s -> (s.var_ctr, { s with var_ctr = s.var_ctr + 1 })

let make_decl ?n_args name =
  let%bind () = add_stmt (Decl (Decl.create ?n_args name)) in
  return (Sexp.Atom name)

let fresh_decl ?n_args ?(prefix = "x") () =
  let%bind ctr = fresh_num in
  let name = sprintf "%s%d" prefix ctr in
  make_decl ?n_args name

let make_defn ?n_args ?ret name body =
  let%bind () = add_stmt (Defn (Defn.create ?n_args name body)) in
  return (Sexp.Atom name)

let fresh_defn ?n_args ?(prefix = "x") body =
  let%bind ctr = fresh_num in
  let name = sprintf "%s%d" prefix ctr in
  make_defn ?n_args name body

let app op args = Sexp.List (Sexp.Atom op :: args)

let annotate key value term =
  Sexp.List [ Sexp.Atom "!"; term; Sexp.Atom (Fmt.str ":%s" key); value ]

let comment = annotate "comment"

module Bool = struct
  let false_ = Sexp.Atom "false"

  let true_ = Sexp.Atom "true"

  let is_false x = [%equal: Sexp.t] x false_

  let is_true x = [%equal: Sexp.t] x true_

  let or_ xs =
    let xs =
      if List.exists ~f:is_true xs then [ true_ ]
      else List.filter ~f:(Fun.negate is_false) xs
    in
    match xs with [] -> false_ | [ x ] -> x | xs -> app "or" xs

  let and_ xs =
    let xs =
      if List.exists ~f:is_false xs then [ false_ ]
      else List.filter ~f:(Fun.negate is_true) xs
    in
    match xs with [] -> true_ | [ x ] -> x | xs -> app "and" xs

  let not_ x =
    if is_true x then false_ else if is_false x then true_ else app "not" [ x ]

  let implies x y =
    if is_true x then y
    else if is_false x || is_true y then true_
    else if is_false y then not_ x
    else app "=>" [ x; y ]

  let ( = ) x y =
    if is_true x then y
    else if is_true y then x
    else if is_false x then not_ y
    else if is_false y then not_ x
    else app "=" [ x; y ]

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

let assert_ expr = add_stmt (Assert expr)

let to_smtlib =
  Fmt.(
    list (fun fmt -> function
      | Decl x -> Decl.to_smtlib fmt x
      | Defn x -> Defn.to_smtlib fmt x
      | Assert x -> Fmt.pf fmt "(assert %a)" Sexp.pp x
      | Extra sexp -> Sexp.pp fmt sexp)
    ++ flush)

module Interpolant = struct
  module Group : sig
    type t

    type 'a s

    val create : t s

    val sexp_of : t -> Sexp.t
  end
  with type 'a s := 'a t = struct
    type t = int

    let create =
      make @@ fun s -> (s.group_ctr, { s with group_ctr = s.group_ctr + 1 })

    let sexp_of x = Sexp.Atom (Fmt.str "g%d" x)
  end

  let assert_group ?group expr =
    let%bind group =
      match group with Some g -> return g | None -> Group.create
    in
    assert_ @@ annotate "interpolation-group" (Group.sexp_of group) expr
end

let with_mathsat f =
  let open Sexp in
  let proc = Unix.open_process "mathsat" in
  let stdout, stdin = proc in
  Out_channel.with_file (debug_out_file ()) ~f:(fun log ->
      let log_fmt = Format.formatter_of_out_channel log in

      let write_buf = Buffer.create 128 in

      let write stmts =
        let fmt = Fmt.with_buffer write_buf in
        to_smtlib fmt stmts;
        Out_channel.output_buffer stdin write_buf;
        Out_channel.flush stdin;

        Out_channel.output_buffer log write_buf;
        Out_channel.newline log;

        Buffer.clear write_buf
      in

      let read () =
        let sexp = Sexp.input_sexp stdout in
        Sexp.to_string_hum sexp |> String.split_lines
        |> List.iter ~f:(Fmt.pf log_fmt "; %s@.");
        sexp
      in

      f read write)

let error sexp =
  Error.create "Unexpected output" sexp [%sexp_of: Sexp.t] |> Error.raise

let get_interpolant_or_model_inner groups stmts read write =
  let open Sexp in
  write
    ( [
        Extra (app "set-option" [ Atom ":produce-interpolants"; Atom "true" ]);
        Extra (app "set-option" [ Atom ":produce-models"; Atom "true" ]);
      ]
    @ stmts
    @ [ Extra (app "check-sat" []) ] );
  let is_sat =
    match read () with
    | Atom "unsat" -> false
    | Atom "sat" -> true
    | x -> error x
  in

  if is_sat then (
    write [ Extra (app "get-model" []) ];
    return (Second (read ())) )
  else (
    write
      [
        Extra
          (app "get-interpolant"
             [ List (List.map ~f:Interpolant.Group.sexp_of groups) ]);
      ];
    return (First (read ())) )

let get_interpolant_or_model groups =
  let open Sexp in
  let%bind stmts = get_stmts in
  with_mathsat @@ get_interpolant_or_model_inner groups stmts

let get_model =
  let open Sexp in
  let%bind stmts = get_stmts in
  with_mathsat @@ fun read write ->
  write
    ( [ Extra (app "set-option" [ Atom ":produce-models"; Atom "true" ]) ]
    @ stmts
    @ [ Extra (app "check-sat" []) ] );
  let is_sat =
    match read () with
    | Atom "unsat" -> false
    | Atom "sat" -> true
    | x -> error x
  in

  if is_sat then (
    write [ Extra (app "get-model" []) ];
    return (Some (read ())) )
  else return None

let check_sat =
  let open Sexp in
  let%bind stmts = get_stmts in
  with_mathsat @@ fun read write ->
  write (stmts @ [ Extra (app "check-sat" []) ]);
  return
    ( match read () with
    | Atom "unsat" -> false
    | Atom "sat" -> true
    | x -> error x )
