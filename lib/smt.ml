open! Core
module Seq = Sequence

module Make () = struct
  module Sort = struct
    type t = Bool

    let to_smtlib fmt = function Bool -> Fmt.pf fmt "Bool"
  end

  module Decl = struct
    type t = { name : string; args : Sort.t list; ret : Sort.t }

    let create ?(args = []) ?(ret = Sort.Bool) name = { name; args; ret }

    let to_smtlib fmt { name; args; ret } =
      Fmt.pf fmt "(declare-fun %s (%a) %a)" name
        Fmt.(list ~sep:sp Sort.to_smtlib)
        args Sort.to_smtlib ret
  end

  module Defn = struct
    type t = Decl.t * Sexp.t

    let create ?args ?ret name body = (Decl.create ?args ?ret name, body)

    let to_smtlib fmt (Decl.{ name; args; ret }, body) =
      Fmt.pf fmt "(define-fun %s (%a) %a %a)" name
        Fmt.(list ~sep:sp Sort.to_smtlib)
        args Sort.to_smtlib ret Sexp.pp_hum body
  end

  let stmts = ref []

  let var_ctr = ref 0

  let add_stmt s = stmts := !stmts @ [ s ]

  let make_decl ?args ?ret name =
    add_stmt (`Decl (Decl.create ?args ?ret name));
    Sexp.Atom name

  let fresh_decl ?args ?ret ?(prefix = "x") () =
    let name = sprintf "%s%d" prefix !var_ctr in
    incr var_ctr;
    make_decl ?args ?ret name

  let make_defn ?args ?ret name body =
    add_stmt (`Defn (Defn.create ?args ?ret name body));
    Sexp.Atom name

  let fresh_defn ?args ?ret ?(prefix = "x") body =
    let name = sprintf "%s%d" prefix !var_ctr in
    incr var_ctr;
    make_defn ?args ?ret name body

  let app op args = Sexp.List (Sexp.Atom op :: args)

  let annotate key value term =
    Sexp.List [ Sexp.Atom "!"; term; Sexp.Atom (Fmt.str ":%s" key); value ]

  let comment = annotate "comment"

  module Bool = struct
    let false_ = Sexp.Atom "false"

    let true_ = Sexp.Atom "true"

    let ( || ) x x' = app "or" [ x; x' ]

    let ( && ) x x' = app "and" [ x; x' ]

    let or_ = function [] -> false_ | xs -> app "or" xs

    let and_ = function [] -> true_ | xs -> app "and" xs

    let not x = app "not" [ x ]

    let ( => ) x y = app "=>" [ x; y ]

    let ( = ) x y = app "=" [ x; y ]

    let at_least_one = or_

    let at_most_one xs =
      let xs = Array.of_list xs and n = List.length xs in
      Seq.init n ~f:(fun i ->
          Seq.range (i + 1) n
          |> Seq.map ~f:(fun j -> (not xs.(i)) || not xs.(j)))
      |> Seq.concat |> Seq.to_list |> and_

    let exactly_one xs = at_least_one xs && at_most_one xs

    let bool x = if x then true_ else false_
  end

  let assert_ expr = add_stmt (`Assert expr)

  let to_smtlib ?stmts:s fmt =
    let stmts = Option.value s ~default:!stmts in
    Fmt.(
      list (fun fmt ->
        function
        | `Decl x -> Decl.to_smtlib fmt x
        | `Defn x -> Defn.to_smtlib fmt x
        | `Assert x -> Fmt.pf fmt "(assert %a)" Sexp.pp_hum x
        | `Extra sexp -> Sexp.pp_hum fmt sexp)
      ++ flush)
      fmt stmts

  let run_mathsat smtlib =
    let proc = Unix.open_process "mathsat" in
    let stdout, stdin = proc in
    Out_channel.output_string stdin smtlib;
    Out_channel.close stdin;
    let sexps = Sexp.input_sexps stdout in
    Unix.close_process proc |> Unix.Exit_or_signal.or_error |> Or_error.ok_exn;
    sexps

  module Interpolant = struct
    module Group : sig
      type t

      val create : unit -> t

      val sexp_of : t -> Sexp.t
    end = struct
      type t = int

      let ctr = ref 0

      let create () =
        incr ctr;
        !ctr

      let sexp_of x = Sexp.Atom (Fmt.str "g%d" x)
    end

    let assert_group ?group expr =
      let group = Option.value group ~default:(Group.create ()) in
      assert_ @@ annotate "interpolation-group" (Group.sexp_of group) expr

    let get_interpolant groups =
      let open Sexp in
      let buf = Buffer.create 128 in
      to_smtlib
        ~stmts:
          ( [
              `Extra
                (app "set-option" [ Atom ":produce-interpolants"; Atom "true" ]);
            ]
          @ !stmts
          @ [
              `Extra (app "check-sat" []);
              `Extra
                (app "get-interpolant"
                   [ List (List.map ~f:Group.sexp_of groups) ]);
            ] )
        (Fmt.with_buffer buf);
      let smtlib = Buffer.contents buf in
      Fmt.pr "Smtlib:\n%s\n\n" smtlib;
      let output = run_mathsat smtlib in
      match output with [ Atom "unsat"; inter ] -> Some inter | _ -> None
  end
end
