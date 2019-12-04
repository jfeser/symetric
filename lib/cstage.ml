open! Core
open! Utils
module Seq = Sequence

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

  type expr = { ebody : string; ret : string; etype : ctype }

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

  type 'a t = expr

  let prog =
    let main =
      {
        fname = "main";
        ftype = Func (Unit, Unit);
        locals = [];
        args = [];
        fbody = { ebody = ""; ret = "0"; etype = Unit };
      }
    in
    { funcs = [ main ]; cur_func = main; fresh = Fresh.create () }

  let find_func n = List.find prog.funcs ~f:(fun f -> String.(f.fname = n))

  type fmt_arg = C : 'a t -> fmt_arg | S : string -> fmt_arg

  let format fmt args =
    let fmt =
      List.fold_left args ~init:fmt ~f:(fun fmt (k, v) ->
          let pat = sprintf "$(%s)" k in
          match v with
          | S v -> String.substr_replace_all fmt ~pattern:pat ~with_:v
          | C v ->
              if String.is_substring fmt ~substring:pat then
                let fmt =
                  String.substr_replace_all fmt ~pattern:pat ~with_:v.ret
                in
                v.ebody ^ fmt
              else fmt)
    in
    ( if String.contains fmt '$' then
      Error.(create "Incomplete template." fmt [%sexp_of: string] |> raise) );
    fmt

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
    { ebody = ""; ret = vname; etype = vtype }

  let add_var_decl x = prog.cur_func.locals <- x :: prog.cur_func.locals

  let fresh_name () = Fresh.name prog.fresh "x%d"

  let fresh_ref vtype init_fmt init_subst =
    let vname = fresh_name () in
    {
      ret = vname;
      ebody =
        format
          ("auto &$(var) = " ^ init_fmt ^ ";")
          ([ ("var", S vname) ] @ init_subst);
      etype = vtype;
    }

  let fresh_local ?init vtype =
    let vname = fresh_name () in
    let ebody =
      match init with
      | Some (init_fmt, init_subst) ->
          format
            ("$(type) $(var) = " ^ init_fmt ^ ";")
            ([ ("type", S (type_name vtype)); ("var", S vname) ] @ init_subst)
      | None ->
          format "$(type) $(var);"
            [ ("type", S (type_name vtype)); ("var", S vname) ]
    in
    { ret = vname; ebody; etype = vtype }

  let fresh_global ?init vtype =
    let vname = fresh_name () in
    add_var_decl { vname; vtype; init = None };
    let ebody =
      match init with
      | Some init ->
          format "$(var) = $(init);" [ ("init", S init); ("var", S vname) ]
      | None -> ""
    in
    { ret = vname; ebody; etype = vtype }

  let ret { ret; _ } = ret

  let of_value etype ret = { ebody = ""; ret; etype }

  let let_ v b =
    let x = b { v with ebody = "" } in
    { x with ebody = v.ebody ^ x.ebody }

  let unop fmt type_ x =
    let_ x (fun x ->
        fresh_local type_ ~init:(sprintf fmt "$(arg)", [ ("arg", S (ret x)) ]))

  let binop fmt type_ x x' =
    let_ x (fun x ->
        let_ x' (fun x' ->
            fresh_local type_
              ~init:
                ( sprintf fmt "$(arg1)" "$(arg2)",
                  [ ("arg1", S (ret x)); ("arg2", S (ret x')) ] )))

  let int x = sprintf "%d" x |> of_value Int

  let bool x = (if x then "1" else "0") |> of_value Bool

  let unit = of_value Unit "0"

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

  let ite cond then_ else_ =
    let ret_var = fresh_global then_.etype in
    let subst =
      [
        ("ret", C ret_var);
        ("cond", C cond);
        ("then", C then_);
        ("else", C else_);
      ]
    in
    {
      ret_var with
      ebody =
        format "if ($(cond)) {" subst
        ^ format "$(ret) = $(then); } else {" subst
        ^ format "$(ret) = $(else); }" subst;
    }

  let seq e e' = { e' with ebody = e.ebody ^ e'.ebody }

  let print s = { unit with ebody = sprintf "std::cout << %S << std::endl;" s }

  let exit = { unit with ebody = "exit(0);" }

  let to_func_t = function Func (t, t') -> (t, t') | _ -> assert false

  let add_func f = prog.funcs <- f :: prog.funcs

  let func name type_ f =
    let in_type, _ = to_func_t type_ in
    let fval = { ret = name; ebody = ""; etype = type_ } in
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
        let_ arg (fun arg ->
            let _, ret_type = to_func_t func.ftype in
            fresh_local ret_type
              ~init:("$(f)($(arg))", [ ("f", S func.fname); ("arg", C arg) ]))
    | None -> failwith (sprintf "No function named %s" f.ret)

  module Array = struct
    let mk_type e =
      let name = sprintf "std::vector<%s >" (type_name e) in
      Array { name; elem_type = e }

    let elem_type = function
      | Array { elem_type; _ } -> elem_type
      | _ -> assert false

    module O = struct
      let ( = ) a a' = binop "(%s == %s)" Bool a a'
    end

    let length x = unop "(%s).size()" Int x

    let is_array_type = function Array _ -> () | _ -> assert false

    let const t a =
      is_array_type t;
      let a = Array.to_list a in
      let name = fresh_name () in
      let assigns =
        List.mapi a ~f:(fun i x ->
            format {|$(name)[$(idx)] = $(val);|}
              [ ("name", S name); ("idx", S (sprintf "%d" i)); ("val", C x) ])
        |> String.concat ~sep:" "
      in
      add_var_decl
        { vname = name; vtype = t; init = Some (int (List.length a)) };
      { ret = name; ebody = assigns; etype = t }

    let init t len f =
      is_array_type t;
      let arr = fresh_global t in
      let_ len (fun len ->
          let idx = fresh_name () in
          let subst =
            [
              ("arr", C arr);
              ("type", S (type_name t));
              ("idx", S idx);
              ("elem_type", S (type_name (elem_type t)));
              ("len", C len);
              ("f_app", C (f (of_value Int idx)));
            ]
          in
          let ebody =
            format "$(arr).clear();" subst
            ^ format "$(arr).reserve($(len));" subst
            ^ format "for(int $(idx) = 0; $(idx) < $(len); $(idx)++) {" subst
            ^ format "$(arr).push_back($(f_app));" subst
            ^ "}"
          in
          { arr with ebody })

    let set a i x =
      let ebody =
        format "$(a)[$(i)] = $(x);" [ ("a", C a); ("i", C i); ("x", C x) ]
      in
      { unit with ebody }

    let get a x =
      fresh_ref (elem_type a.etype) "($(a)[$(x)])" [ ("a", C a); ("x", C x) ]

    let map t arr ~f = init t (length arr) (fun i -> let_ (get arr i) f)

    let map2 t a1 a2 ~f = init t (length a1) (fun i -> f (get a1 i) (get a2 i))

    let sub a s l = init a.etype (max (int 0) (l - s)) (fun i -> get a (s + i))

    let fold arr ~init ~f =
      let iter = fresh_global Int in
      let acc = fresh_global init.etype in
      let_ init (fun init ->
          let_ arr (fun arr ->
              let subst =
                [
                  ("init", C init);
                  ("acc", C acc);
                  ("len", C (length arr));
                  ("f_app", C (f acc (get arr iter)));
                  ("iter", C iter);
                ]
              in
              let ebody =
                format
                  {| $(acc) = $(init); for($(iter) = 0; $(iter) < $(len); $(iter)++) { |}
                  subst
                ^ format {|$(acc) = $(f_app);|} subst
                ^ "}"
              in
              { acc with ebody }))
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
      { ret = set; ebody = ""; etype = ctype }

    let iter a f =
      let iter = fresh_name () in
      let_ a (fun a ->
          let f_app = f (of_value (elem_type a.etype) (sprintf "*%s" iter)) in
          let subst = [ ("name", C a); ("f_app", C f_app); ("iter", S iter) ] in
          let ebody =
            format
              "for(auto $(iter) = $(name).begin(); $(iter) != $(name).end(); \
               ++$(iter)) {"
              subst
            ^ f_app.ebody ^ "}"
          in
          { unit with ebody })

    let add a x =
      let_ a (fun a ->
          let_ x (fun x ->
              {
                unit with
                ebody =
                  format "$(name).insert($(val));"
                    [ ("name", C a); ("val", C x) ];
              }))
  end

  module Tuple = struct
    let mk_type x y =
      let name = sprintf "std::pair<%s,%s >" (type_name x) (type_name y) in
      Tuple (name, x, y)

    let fst_type = function Tuple (_, x, _) -> x | _ -> assert false

    let snd_type = function Tuple (_, _, x) -> x | _ -> assert false

    let create x y =
      let type_ = mk_type x.etype y.etype in
      let var_ = fresh_local type_ in
      let ebody =
        format "$(var) = std::make_pair($(x), $(y));"
          [ ("var", C var_); ("x", C x); ("y", C y) ]
      in
      { var_ with ebody }

    let fst t =
      let type_ = fst_type t.etype in
      fresh_ref type_ "std::get<0>($(t))" [ ("t", C t) ]

    let snd t =
      let type_ = snd_type t.etype in
      fresh_ref type_ "std::get<1>($(t))" [ ("t", C t) ]
  end
end

let%expect_test "" =
  let module C = Code () in
  let open C in
  let open Array in
  fold (init (mk_type Int) (int 10) (fun i -> i)) ~init:(int 0) ~f:( + )
  |> to_string |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int main();
    std::vector<int> x0;
    int x2;
    int x3;
    int main() {
      x0.clear();
      x0.reserve(10);
      for (int x1 = 0; x1 < 10; x1++) {
        x0.push_back(x1);
      }
      int x6 = (x0).size();
      x3 = 0;
      for (x2 = 0; x2 < x6; x2++) {
        auto &x4 = (x0[x2]);
        int x5 = (x3 + x4);
        x3 = x5;
      }
      return x3;
    } |}]

let%expect_test "" =
  let module C = Code () in
  let open C in
  let f = func "f" (Func (Int, Int)) (fun i -> i + int 1) in
  let g = func "g" (Func (Int, Int)) (fun i -> i - int 1) in
  apply f (apply g (int 0)) |> to_string |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int g(int &x2);
    int f(int &x0);
    int main();
    int main() {
      int x4 = g(0);
      int x5 = f(x4);
      return x5;
    }
    int f(int &x0) {
      int x1 = (x0 + 1);
      return x1;
    }
    int g(int &x2) {
      int x3 = (x2 - 1);
      return x3;
    } |}]

let%expect_test "" =
  let module C = Code () in
  let open C in
  let int_array = Array.mk_type Int in
  let f = func "f" (Func (int_array, Int)) (fun a -> a.(int 0)) in
  apply f (Array.init int_array (int 10) (fun i -> i))
  |> to_string |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int f(std::vector<int> &x0);
    int main();
    std::vector<int> x2;
    int main() {
      x2.clear();
      x2.reserve(10);
      for (int x3 = 0; x3 < 10; x3++) {
        x2.push_back(x3);
      }
      int x4 = f(x2);
      return x4;
    }
    int f(std::vector<int> &x0) {
      auto &x1 = (x0[0]);
      return x1;
    } |}]

let%expect_test "" =
  let module C = Code () in
  let open C in
  let int_array = Array.mk_type Int in
  let f =
    let x =
      Tuple.create
        (Array.init int_array (int 10) (fun i -> i))
        (Array.init int_array (int 10) (fun i -> i))
    in
    let y = Tuple.fst x in
    y.(int 5) + y.(int 4)
  in
  f |> to_string |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <iostream>
    #include <set>
    #include <vector>
    int main();
    std::vector<int> x0;
    std::vector<int> x2;
    int main() {
      x0.clear();
      x0.reserve(10);
      for (int x1 = 0; x1 < 10; x1++) {
        x0.push_back(x1);
      }
      x2.clear();
      x2.reserve(10);
      for (int x3 = 0; x3 < 10; x3++) {
        x2.push_back(x3);
      }
      std::pair<std::vector<int>, std::vector<int>> x4;
      x4 = std::make_pair(x2, x0);
      auto &x5 = std::get<0>(x4);
      auto &x7 = (x5[5]);
      x0.clear();
      x0.reserve(10);
      for (int x1 = 0; x1 < 10; x1++) {
        x0.push_back(x1);
      }
      x2.clear();
      x2.reserve(10);
      for (int x3 = 0; x3 < 10; x3++) {
        x2.push_back(x3);
      }
      std::pair<std::vector<int>, std::vector<int>> x4;
      x4 = std::make_pair(x2, x0);
      auto &x5 = std::get<0>(x4);
      auto &x6 = (x5[4]);
      int x8 = (x7 + x6);
      return x8;
    }
 |}]
