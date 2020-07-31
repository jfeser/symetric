open! Core

let debug_out_file =
  let ctr = ref 0 in
  fun () ->
    incr ctr;
    sprintf "interp%d.smt2" !ctr

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
      if is_true x then false_
      else if is_false x then true_
      else app "not" [ x ]

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

    let get_interpolant_or_model groups =
      let open Sexp in
      let proc = Unix.open_process "mathsat" in
      let stdout, stdin = proc in
      Out_channel.with_file (debug_out_file ()) ~f:(fun log ->
          let log_fmt = Format.formatter_of_out_channel log in
          let write stmts =
            let buf = Buffer.create 128 in
            let fmt = Fmt.with_buffer buf in
            to_smtlib ~stmts fmt;
            let str = Buffer.contents buf in
            Out_channel.output_string stdin str;
            Out_channel.flush stdin;
            Fmt.pf log_fmt "%s@." str
          in

          let read () =
            let sexp = Sexp.input_sexp stdout in
            let buf = Buffer.create 128 in
            let fmt = Fmt.with_buffer buf in
            Sexp.pp_hum fmt sexp;
            Buffer.contents buf |> String.split_lines
            |> List.map ~f:(sprintf "; %s")
            |> String.concat ~sep:"\n" |> Fmt.pf log_fmt "%s@.";
            sexp
          in

          let error sexp =
            Error.create "Unexpected output" sexp [%sexp_of: Sexp.t]
            |> Error.raise
          in

          write
            ( [
                `Extra
                  (app "set-option"
                     [ Atom ":produce-interpolants"; Atom "true" ]);
                `Extra
                  (app "set-option" [ Atom ":produce-models"; Atom "true" ]);
              ]
            @ !stmts
            @ [ `Extra (app "check-sat" []) ] );
          let is_sat =
            match read () with
            | Atom "unsat" -> false
            | Atom "sat" -> true
            | x -> error x
          in

          if is_sat then (
            write [ `Extra (app "get-model" []) ];
            Second (read ()) )
          else (
            write
              [
                `Extra
                  (app "get-interpolant"
                     [ List (List.map ~f:Group.sexp_of groups) ]);
              ];
            First (read ()) ))
  end
end
