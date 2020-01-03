open! Core
open Utils

module Term = struct
  type 'a t = Nonterm of 'a | App of string * 'a t list
  [@@deriving compare, hash, sexp]

  let rec size = function
    | Nonterm _ -> 1
    | App (_, ts) -> 1 + List.sum (module Int) ~f:size ts

  let rec non_terminals = function
    | Nonterm x -> [ x ]
    | App (_, ts) -> List.concat_map ~f:non_terminals ts

  let n_holes t = non_terminals t |> List.length

  let rec to_string = function
    | Nonterm x -> x
    | App (f, xs) ->
        List.map xs ~f:to_string |> String.concat ~sep:", "
        |> sprintf "%s(%s)" f

  let with_holes ?fresh ~equal t =
    let fresh = Option.value fresh ~default:(Fresh.create ()) in
    let nt = non_terminals t in
    let holes = ref [] in
    let rec rename = function
      | Nonterm v ->
          if List.mem nt v ~equal then (
            let v' = v ^ Fresh.name fresh "%d" in
            holes := (v, v') :: !holes;
            App (v', []) )
          else Nonterm v
      | App (f, ts) -> App (f, List.map ts ~f:rename)
    in
    let t' = rename t in
    (t', !holes)
end

type nonterm = string [@@deriving compare, sexp]

type t = (nonterm * nonterm Term.t) list [@@deriving compare, sexp]

let rhs g s =
  List.filter_map g ~f:(fun (s', t) -> if String.(s = s') then Some t else None)

let non_terminals g =
  List.map g ~f:(fun (x, _) -> x)
  |> List.dedup_and_sort ~compare:[%compare: nonterm]

let rec product = function
  | [] -> []
  | [ s ] -> List.map ~f:(fun x -> [ x ]) s
  | s :: ss ->
      product ss
      |> List.concat_map ~f:(fun xs -> List.map s ~f:(fun x -> x :: xs))

let inline sym g =
  let rhs =
    List.filter_map g ~f:(fun (s, r) ->
        if [%compare.equal: nonterm] sym s then Some r else None)
  in
  let rec subst_all = function
    | Term.Nonterm x as t ->
        if [%compare.equal: nonterm] sym x then rhs else [ t ]
    | App (_, []) as t -> [ t ]
    | App (f, ts) ->
        List.map ts ~f:subst_all |> product
        |> List.map ~f:(fun ts -> Term.App (f, ts))
  in
  List.concat_map g ~f:(fun (lhs, rhs) ->
      subst_all rhs |> List.map ~f:(fun rhs' -> (lhs, rhs')))

let%expect_test "" =
  inline "A" [ ("A", App ("x", [])); ("B", App ("f", [ Nonterm "A" ])) ]
  |> [%sexp_of: t] |> print_s;
  [%expect {| ((A (App x ())) (B (App f ((App x ()))))) |}]

let%expect_test "" =
  inline "A"
    [
      ("A", App ("x", []));
      ("A", App ("y", []));
      ("B", App ("f", [ Nonterm "A" ]));
    ]
  |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    ((A (App x ())) (A (App y ())) (B (App f ((App x ()))))
     (B (App f ((App y ()))))) |}]

let%expect_test "" =
  let nt x = Term.Nonterm x in
  let id x = Term.App (x, []) in

  inline "FII"
    [
      ("I", App ("head", [ nt "L" ]));
      ("I", App ("last", [ nt "L" ]));
      ("L", App ("take", [ nt "I"; nt "L" ]));
      ("L", App ("drop", [ nt "I"; nt "L" ]));
      ("I", App ("access", [ nt "I"; nt "L" ]));
      ("I", App ("minimum", [ nt "L" ]));
      ("I", App ("maximum", [ nt "L" ]));
      ("L", App ("reverse", [ nt "L" ]));
      ("I", App ("sum", [ nt "L" ]));
      ("L", App ("map", [ nt "FII"; nt "L" ]));
      ("I", App ("count", [ nt "FIB"; nt "L" ]));
      ("L", App ("zipwith", [ nt "FIII"; nt "L"; nt "L" ]));
      ("FII", id "(+1)");
      ("FII", id "(-1)");
      ("FII", id "(*2)");
      ("FII", id "(/2)");
      ("FII", id "(*(-1))");
      ("FII", id "(**2)");
      ("FII", id "(*3)");
      ("FII", id "(/3)");
      ("FII", id "(*4)");
      ("FII", id "(/4)");
      ("FIB", id "(>0)");
      ("FIB", id "(<0)");
      ("FIB", id "(%2==0)");
      ("FIB", id "(%2==1)");
      ("FIII", id "(+)");
      ("FIII", id "(-)");
      ("FIII", id "(*)");
      ("FIII", id "min");
      ("FIII", id "max");
    ]
  |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    ((I (App head ((Nonterm L)))) (I (App last ((Nonterm L))))
     (L (App take ((Nonterm I) (Nonterm L))))
     (L (App drop ((Nonterm I) (Nonterm L))))
     (I (App access ((Nonterm I) (Nonterm L)))) (I (App minimum ((Nonterm L))))
     (I (App maximum ((Nonterm L)))) (L (App reverse ((Nonterm L))))
     (I (App sum ((Nonterm L)))) (L (App map ((App "(+1)" ()) (Nonterm L))))
     (L (App map ((App "(-1)" ()) (Nonterm L))))
     (L (App map ((App "(*2)" ()) (Nonterm L))))
     (L (App map ((App "(/2)" ()) (Nonterm L))))
     (L (App map ((App "(*(-1))" ()) (Nonterm L))))
     (L (App map ((App "(**2)" ()) (Nonterm L))))
     (L (App map ((App "(*3)" ()) (Nonterm L))))
     (L (App map ((App "(/3)" ()) (Nonterm L))))
     (L (App map ((App "(*4)" ()) (Nonterm L))))
     (L (App map ((App "(/4)" ()) (Nonterm L))))
     (I (App count ((Nonterm FIB) (Nonterm L))))
     (L (App zipwith ((Nonterm FIII) (Nonterm L) (Nonterm L))))
     (FII (App "(+1)" ())) (FII (App "(-1)" ())) (FII (App "(*2)" ()))
     (FII (App "(/2)" ())) (FII (App "(*(-1))" ())) (FII (App "(**2)" ()))
     (FII (App "(*3)" ())) (FII (App "(/3)" ())) (FII (App "(*4)" ()))
     (FII (App "(/4)" ())) (FIB (App "(>0)" ())) (FIB (App "(<0)" ()))
     (FIB (App "(%2==0)" ())) (FIB (App "(%2==1)" ())) (FIII (App "(+)" ()))
     (FIII (App "(-)" ())) (FIII (App "(*)" ())) (FIII (App min ()))
     (FIII (App max ()))) |}]

let with_holes ?fresh = Term.with_holes ?fresh ~equal:[%compare.equal: nonterm]
