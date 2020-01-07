open! Core
module Seq = Sequence

let with_stackmark body =
  let open Delimcc in
  let p = new_prompt () in
  push_prompt p (fun () -> body (fun () -> is_prompt_set p))

module Code () : Sigs.CODE = struct
  type 'a set = Set

  type ntype = { name : string; elem_type : ctype }

  and ctype =
    | Unit
    | Int
    | Bool
    | Array of ntype
    | Set of ntype
    | Tuple of string * ctype * ctype
    | Func of ctype * ctype
  [@@deriving compare, sexp]

  let type_name = function
    | Unit -> "int"
    | Int -> "int"
    | Bool -> "int"
    | Tuple (n, _, _) -> n
    | Array { name; _ } | Set { name; _ } -> name
    | _ -> failwith "Type cannot be constructed."

  type mark = unit -> bool

  type expr = {
    ebody : string;
    ret : string;
    etype : ctype;
    efree : (string * (mark[@opaque])) list;
    eeffect : bool;
  }
  [@@deriving sexp]

  type var_decl = { vname : string; vtype : ctype; init : expr option }

  type func_decl = {
    fname : string;
    ftype : ctype;
    mutable locals : var_decl list;
    mutable args : var_decl list;
    mutable fbody : expr;
  }

  type prog = {
    mutable funcs : func_decl list;
    mutable cur_func : func_decl;
    fresh : Fresh.t;
  }

  module C = struct
    type 'a t = expr

    let sexp_of_t = sexp_of_expr

    let is_well_scoped c =
      (not c.eeffect) && List.for_all c.efree ~f:(fun (_, m) -> m ())

    let let_ v b =
      let x = b { v with ebody = "" } in
      { x with ebody = v.ebody ^ x.ebody; efree = x.efree @ v.efree }
  end

  include C
  include Genlet.Make (C)

  let sexp_of_t _ = sexp_of_t

  let free { efree; _ } = efree

  let prog =
    let main =
      {
        fname = "main";
        ftype = Func (Unit, Unit);
        locals = [];
        args = [];
        fbody =
          { ebody = ""; ret = "0"; etype = Unit; efree = []; eeffect = false };
      }
    in
    { funcs = [ main ]; cur_func = main; fresh = Fresh.create () }

  let with_comment s e =
    { e with ebody = sprintf "\n// begin %s\n%s// end %s\n" s e.ebody s }

  let find_func n = List.find prog.funcs ~f:(fun f -> String.(f.fname = n))

  type fmt_arg = C : 'a t -> fmt_arg | S : string -> fmt_arg

  let format fmt args =
    let fmt =
      List.fold_left args ~init:fmt ~f:(fun fmt (k, v) ->
          let pat = sprintf "$(%s)" k in
          match v with
          | S v -> String.substr_replace_all fmt ~pattern:pat ~with_:v
          | C v -> String.substr_replace_all fmt ~pattern:pat ~with_:v.ret)
    in
    ( if String.contains fmt '$' then
      Error.(create "Incomplete template." fmt [%sexp_of: string] |> raise) );
    fmt

  let eformat ?(has_effect = false) ret_fmt etype body_fmt args =
    let ebody =
      let arg_bodies =
        List.map args ~f:(fun (_, arg) ->
            match arg with C v -> v.ebody | _ -> "")
        |> String.concat ~sep:" "
      in
      arg_bodies ^ format body_fmt args
    in
    let ret = format ret_fmt args in
    let efree =
      List.concat_map args ~f:(function _, C { efree; _ } -> efree | _ -> [])
    in
    let eeffect =
      List.exists args ~f:(function
        | _, C { eeffect; _ } -> eeffect
        | _ -> false)
    in
    { ebody; ret; etype; efree; eeffect = has_effect || eeffect }

  let seq e e' =
    {
      ebody = e.ebody ^ e'.ebody;
      etype = e'.etype;
      ret = e'.ret;
      efree = e.efree @ e'.efree;
      eeffect = e.eeffect || e'.eeffect;
    }

  let type_of e = e.etype

  let cast x = x

  let to_string e =
    let expr_to_str e = e.ebody in
    let var_decl_to_str v =
      let subst = [ ("type", S (type_name v.vtype)); ("name", S v.vname) ] in
      match v.init with
      | Some init ->
          format "$(type) $(name) ($(init));" (("init", C init) :: subst)
      | None -> format "$(type) $(name);" subst
    in
    let arg_to_str v = sprintf "%s &%s" (type_name v.vtype) v.vname in
    let func_to_decl_str f =
      let ret_type =
        match f.ftype with Func (_, t) -> t | _ -> assert false
      in
      let args_str = List.map f.args ~f:arg_to_str |> String.concat ~sep:"," in
      sprintf "%s %s(%s);" (type_name ret_type) f.fname args_str
    in
    let func_to_def_str f =
      let ret_type =
        match f.ftype with Func (_, t) -> t | _ -> assert false
      in
      let locals_str =
        List.rev f.locals
        |> List.map ~f:var_decl_to_str
        |> String.concat ~sep:" "
      in
      let args_str = List.map f.args ~f:arg_to_str |> String.concat ~sep:"," in
      if String.(f.fname = "main") then
        sprintf "%s %s(%s) { %s return %s; }" (type_name ret_type) f.fname
          args_str (expr_to_str f.fbody) f.fbody.ret
      else
        sprintf "%s %s(%s) { %s %s return %s; }" (type_name ret_type) f.fname
          args_str locals_str (expr_to_str f.fbody) f.fbody.ret
    in
    prog.cur_func.fbody <-
      { e with ebody = prog.cur_func.fbody.ebody ^ e.ebody };
    let header = "#include <vector>\n#include <set>\n#include <iostream>\n" in
    let forward_decls =
      List.map prog.funcs ~f:func_to_decl_str |> String.concat ~sep:"\n"
    in
    let main_decls =
      find_func "main"
      |> Option.map ~f:(fun func ->
             List.rev func.locals
             |> List.map ~f:var_decl_to_str
             |> String.concat ~sep:" ")
      |> Option.value ~default:""
    in
    let funcs =
      List.rev prog.funcs
      |> List.map ~f:func_to_def_str
      |> String.concat ~sep:"\n"
    in
    header ^ forward_decls ^ main_decls ^ funcs

  let expr_of_var { vname; vtype; _ } =
    { ebody = ""; ret = vname; etype = vtype; efree = []; eeffect = false }

  let add_var_decl x = prog.cur_func.locals <- x :: prog.cur_func.locals

  let fresh_name () = Fresh.name prog.fresh "x%d"

  let fresh_global ?init vtype =
    let vname = fresh_name () in
    add_var_decl { vname; vtype; init = None };
    match init with
    | Some init ->
        eformat vname vtype "$(var) = $(init);"
          [ ("init", S init); ("var", S vname) ]
    | None ->
        { ret = vname; ebody = ""; etype = vtype; efree = []; eeffect = false }

  let fresh_var etype m =
    let name = Fresh.name prog.fresh "x%d" in
    { ret = name; etype; efree = [ (name, m) ]; ebody = ""; eeffect = false }

  let unop fmt type_ x =
    eformat (sprintf fmt "$(arg)") type_ "" [ ("arg", C x) ]

  let binop fmt type_ x x' =
    eformat
      (sprintf fmt "$(arg1)" "$(arg2)")
      type_ ""
      [ ("arg1", C x); ("arg2", C x') ]

  let int x = eformat (sprintf "%d" x) Int "" []

  let bool x = eformat (if x then "1" else "0") Bool "" []

  let unit = eformat "0" Unit "" []

  let rec seq_many = function [] -> unit | x :: xs -> seq x (seq_many xs)

  let ( ~- ) x = unop "(-%s)" Int x

  let ( + ) x y = binop "(%s + %s)" Int x y

  let ( - ) x y = binop "(%s - %s)" Int x y

  let ( * ) x y = binop "(%s * %s)" Int x y

  let ( / ) x y = binop "(%s / %s)" Int x y

  let ( mod ) x y = binop "(%s %% %s)" Int x y

  let ( = ) x y = binop "(%s == %s)" Bool x y

  let ( > ) x y = binop "(%s > %s)" Bool x y

  let ( < ) x y = binop "(%s < %s)" Bool x y

  let ( <= ) x y = binop "(%s <= %s)" Bool x y

  let ( >= ) x y = binop "(%s >= %s)" Bool x y

  let ( && ) x y = binop "(%s && %s)" Bool x y

  let ( || ) x y = binop "(%s || %s)" Bool x y

  let not x = unop "(!%s)" Bool x

  let min x y = binop "std::min(%s, %s)" Int x y

  let max x y = binop "std::max(%s, %s)" Int x y

  let rec sseq = function [] -> unit | [ x ] -> x | x :: xs -> seq x (sseq xs)

  let print s =
    eformat ~has_effect:true "0" Unit "std::cout << $(str) << std::endl;"
      [ ("str", S (sprintf "%S" s)) ]

  let exit = eformat ~has_effect:true "0" Unit "exit(0);" []

  let to_func_t = function Func (t, t') -> (t, t') | _ -> assert false

  let add_func f = prog.funcs <- f :: prog.funcs

  let func name type_ f =
    let in_type, _ = to_func_t type_ in
    let fval =
      { ret = name; ebody = ""; etype = type_; efree = []; eeffect = false }
    in
    if Option.is_none (find_func name) then (
      let arg = { vname = fresh_name (); vtype = in_type; init = None } in
      let func =
        {
          fname = name;
          ftype = type_;
          locals = [];
          args = [ arg ];
          fbody = unit;
        }
      in
      add_func func;
      let old_func = prog.cur_func in
      prog.cur_func <- func;
      func.fbody <- f (expr_of_var arg);
      prog.cur_func <- old_func );

    fval

  let apply f arg =
    match find_func f.ret with
    | Some func ->
        let _, ret_type = to_func_t func.ftype in
        eformat ~has_effect:true "$(f)($(arg))" ret_type ""
          [ ("f", S func.fname); ("arg", C arg) ]
    | None -> failwith (sprintf "No function named %s" f.ret)

  let assign x v =
    eformat ~has_effect:true "0" Unit {|$(v) = $(x);|}
      [ ("x", C x); ("v", C v) ]

  let ite cond then_ else_ =
    let_locus @@ fun () ->
    let then_ = let_locus then_ in
    let else_ = let_locus else_ in
    let ret_var = fresh_global then_.etype in
    eformat ret_var.ret ret_var.etype
      {|
if ($(cond)) {
  $(then)
  $(ret) = $(then_ret);
} else {
  $(else)
  $(ret) = $(else_ret); }
|}
      [
        ("ret", C ret_var);
        ("cond", C cond);
        ("then", S then_.ebody);
        ("else", S else_.ebody);
        ("then_ret", S then_.ret);
        ("else_ret", S else_.ret);
      ]

  let for_ lo step hi f =
    let_locus @@ fun () ->
    let i, body =
      with_stackmark (fun m ->
          let i = fresh_var Int m in
          let body = let_locus @@ fun () -> f i in
          (i, body))
    in
    eformat "0" Unit
      {|
for(int $(i) = $(lo); $(i) < $(hi); $(i)++) {
      $(body)
}
|}
      [
        ("lo", C lo);
        ("step", C step);
        ("hi", C hi);
        ("body", S body.ebody);
        ("i", C i);
      ]

  module Array = struct
    let mk_type e =
      let name = sprintf "std::vector<%s >" (type_name e) in
      Array { name; elem_type = e }

    let elem_type = function
      | Array { elem_type; _ } -> elem_type
      | t ->
          Error.create "Expected an array type." t [%sexp_of: ctype]
          |> Error.raise

    module O = struct
      let ( = ) a a' = binop "(%s == %s)" Bool a a'
    end

    let length x = unop "((int)((%s).size()))" Int x

    let is_array_type = function Array _ -> () | _ -> assert false

    let const t a =
      is_array_type t;
      let a = Array.to_list a in
      let name = fresh_name () in
      add_var_decl
        { vname = name; vtype = t; init = Some (int (List.length a)) };
      let assigns =
        List.mapi a ~f:(fun i x ->
            eformat "0" Unit {|$(name)[$(idx)] = $(val);|}
              [ ("name", S name); ("idx", S (sprintf "%d" i)); ("val", C x) ])
        |> sseq
      in
      {
        assigns with
        ret = name;
        etype = t;
        efree = List.concat_map a ~f:free;
        eeffect = false;
      }
      |> with_comment "Array.const"

    let clear a =
      eformat ~has_effect:true "0" Unit "$(a).clear();" [ ("a", C a) ]

    let reserve a n =
      eformat ~has_effect:true "0" Unit "$(a).reserve($(n));"
        [ ("a", C a); ("n", C n) ]

    let push_back a x =
      eformat ~has_effect:true "0" Unit "$(a).push_back($(x));"
        [ ("a", C a); ("x", C x) ]

    let init t len f =
      let a = fresh_global t in
      sseq
        [
          clear a;
          reserve a len;
          for_ (int 0) (int 1) len (fun i -> push_back a (genlet (f i)));
          a;
        ]
      |> genlet |> with_comment "Array.init"

    let set a i x =
      eformat ~has_effect:true "0" Unit "$(a)[$(i)] = $(x);"
        [ ("a", C a); ("i", C i); ("x", C x) ]

    let get a x =
      eformat "($(a)[$(x)])" (elem_type a.etype) "" [ ("a", C a); ("x", C x) ]

    let map t arr ~f = init t (length arr) (fun i -> let_ (get arr i) f)

    let map2 t a1 a2 ~f = init t (length a1) (fun i -> f (get a1 i) (get a2 i))

    let sub a s l = init a.etype (max (int 0) (l - s)) (fun i -> get a (s + i))

    let fold arr ~init ~f =
      let acc = fresh_global init.etype in
      sseq
        [
          assign init acc;
          for_ (int 0) (int 1) (length arr) (fun i ->
              assign (f acc (get arr i)) acc);
          acc;
        ]
      |> with_comment "Array.fold"
  end

  module Set = struct
    let mk_type e =
      let name = sprintf "std::set<%s >" (type_name e) in
      Set { name; elem_type = e }

    let elem_type = function
      | Set { elem_type; _ } -> elem_type
      | _ -> assert false

    let empty ctype =
      let set = fresh_name () in
      add_var_decl { vname = set; vtype = ctype; init = None };
      { ret = set; ebody = ""; etype = ctype; efree = []; eeffect = true }

    let iter a f =
      let iter_name = fresh_name () in
      let_locus @@ fun () ->
      let body =
        with_stackmark (fun m ->
            let arg =
              {
                ret = sprintf "*%s" iter_name;
                ebody = "";
                etype = elem_type a.etype;
                efree = [ (iter_name, m) ];
                eeffect = false;
              }
            in
            let_locus @@ fun () -> f arg)
      in
      eformat "0" Unit
        {|
for(auto $(iter) = $(set).begin(); $(iter) != $(set).end(); ++$(iter)) {
        $(body)
}
|}
        [ ("set", C a); ("iter", S iter_name); ("body", S body.ebody) ]

    let fold a ~init ~f =
      let_ (fresh_global init.etype) (fun acc ->
          sseq [ assign init acc; iter a (fun x -> assign (f acc x) acc); acc ])
      |> with_comment "Set.fold"

    let add a x =
      eformat ~has_effect:true "0" Unit "$(name).insert($(val));"
        [ ("name", C a); ("val", C x) ]
  end

  module Tuple = struct
    let mk_type x y =
      let name = sprintf "std::pair<%s,%s >" (type_name x) (type_name y) in
      Tuple (name, x, y)

    let fst_type = function Tuple (_, x, _) -> x | _ -> assert false

    let snd_type = function Tuple (_, _, x) -> x | _ -> assert false

    let create x y =
      let type_ = mk_type x.etype y.etype in
      eformat "std::make_pair($(x), $(y))" type_ "" [ ("x", C x); ("y", C y) ]

    let fst t = eformat "std::get<0>($(t))" (fst_type t.etype) "" [ ("t", C t) ]

    let snd t = eformat "std::get<1>($(t))" (snd_type t.etype) "" [ ("t", C t) ]
  end
end
