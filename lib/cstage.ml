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

  let to_string e =
    let expr_to_str e = e.ebody in
    let var_decl_to_str v =
      let subst = [ ("type", S (type_name v.vtype)); ("name", S v.vname) ] in
      match v.init with
      | Some init ->
          format "$(type) $(name) ($(init));" (("init", C init) :: subst)
      | None -> format "$(type) $(name);" subst
    in
    let arg_to_str v = sprintf "%s %s" (type_name v.vtype) v.vname in
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
      sprintf "%s %s(%s) { %s %s return %s; }" (type_name ret_type) f.fname
        args_str locals_str (expr_to_str f.fbody) f.fbody.ret
    in
    prog.cur_func.fbody <-
      { e with ebody = prog.cur_func.fbody.ebody ^ e.ebody };
    let header = "#include <vector>\n#include <set>\n" in
    let forward_decls =
      List.map prog.funcs ~f:func_to_decl_str |> String.concat ~sep:"\n"
    in
    let funcs =
      List.rev prog.funcs
      |> List.map ~f:func_to_def_str
      |> String.concat ~sep:"\n"
    in
    header ^ forward_decls ^ funcs

  let expr_of_var { vname; vtype; _ } =
    { ebody = ""; ret = vname; etype = vtype }

  let add_var_decl x = prog.cur_func.locals <- x :: prog.cur_func.locals

  let fresh_name () = Fresh.name prog.fresh "x%d"

  let fresh_var vtype =
    let vname = fresh_name () in
    add_var_decl { vname; vtype; init = None };
    { ret = vname; ebody = ""; etype = vtype }

  let ret { ret; _ } = ret

  let assign = sprintf "%s = %s;"

  let of_value type_ x =
    let var_ = fresh_var type_ in
    { var_ with ebody = var_.ebody ^ assign var_.ret x }

  let let_ v b =
    let x = b { v with ebody = "" } in
    { x with ebody = v.ebody ^ x.ebody }

  let unop fmt type_ x =
    let var_ = fresh_var type_ in
    {
      var_ with
      ebody = var_.ebody ^ x.ebody ^ assign var_.ret (sprintf fmt (ret x));
    }

  let binop fmt type_ x x' =
    let var_ = fresh_var type_ in
    let_ x (fun x ->
        let_ x' (fun x' ->
            {
              var_ with
              ebody =
                var_.ebody ^ x.ebody ^ x'.ebody
                ^ assign var_.ret (sprintf fmt (ret x) (ret x'));
            }))

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

  let ite cond then_ else_ =
    let ret_var = fresh_var then_.etype in
    {
      ret_var with
      ebody =
        format "if ($(cond)) { $(ret) = $(then); } else { $(ret) = $(else); }"
          [
            ("ret", C ret_var);
            ("cond", C cond);
            ("then", C then_);
            ("else", C else_);
          ];
    }

  let seq e e' = { e' with ebody = e.ebody ^ e'.ebody }

  let print s = { unit with ebody = sprintf "std::cout << %S << endl;" s }

  let to_func_t = function Func (t, t') -> (t, t') | _ -> assert false

  let find_func n = List.find prog.funcs ~f:(fun f -> String.(f.fname = n))

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
        let _, ret_type = to_func_t func.ftype in
        let var_ = fresh_var ret_type in
        {
          var_ with
          ebody =
            format "$(var) = $(f)($(arg));"
              [ ("var", S var_.ret); ("f", S func.fname); ("arg", C arg) ];
        }
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

    let const t a =
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
      let iter = fresh_var Int in
      let name = fresh_name () in
      let f_app = f iter in
      let subst =
        [
          ("name", S name);
          ("type", S (type_name t));
          ("elem_type", S (type_name (elem_type t)));
          ("len", C len);
          ("f_app", C f_app);
        ]
      in
      add_var_decl { vname = name; vtype = t; init = Some len };
      let ebody =
        format "for(int i = 0; i < $(len); i++) {\n" subst
        ^ format "$(name)[i] = $(f_app);\n" subst
        ^ "}\n"
      in
      { ret = name; ebody; etype = t }

    let set a i x =
      let ebody =
        format "$(a)[$(i)] = $(x);" [ ("a", C a); ("i", C i); ("x", C x) ]
      in
      { unit with ebody }

    let get a = binop "(%s[%s])" (elem_type a.etype) a

    let sub a s l = init a.etype (l - s) (fun i -> get a (s + i))

    let fold a ~init ~f =
      let acc = fresh_var init.etype in
      let iter = fresh_var Int in
      let_ a (fun a ->
          let f_app = f acc (get a iter) in
          let len = length a in
          let subst =
            [
              ("init", C init);
              ("acc", C acc);
              ("len", C len);
              ("f_app", C f_app);
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
          { acc with ebody })
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
          let subst =
            [ ("name", C a); ("f_app", C f_app); ("iter", S iter) ]
          in
          let ebody =
            format
              "for(auto $(iter) = $(name).begin(); $(iter) != $(name).end(); \
               ++$(iter)) {"
              subst
            ^ format {|$(f_app);|} subst ^ "}"
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
      let var_ = fresh_var type_ in
      let ebody =
        format "$(var) = std::make_pair($(x), $(y));"
          [ ("var", C var_); ("x", C x); ("y", C y) ]
      in
      { var_ with ebody }

    let fst t =
      let type_ = fst_type t.etype in
      let var_ = fresh_var type_ in
      let ebody =
        format "$(var) = std::get<0>($(t));" [ ("var", C var_); ("t", C t) ]
      in
      { var_ with ebody }

    let snd t =
      let type_ = snd_type t.etype in
      let var_ = fresh_var type_ in
      let ebody =
        format "$(var) = std::get<1>($(t));" [ ("var", C var_); ("t", C t) ]
      in
      { var_ with ebody }
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
    #include <set>
    #include <vector>
    int main() {
      int x0;
      int x1;
      int x2;
      int x3;
      x2 = 10;
      std::vector<int> x4(x2);
      int x5;
      int x6;
      int x7;
      int x8;
      int x9;
      x2 = 10;
      for (int i = 0; i < x2; i++) {
        x4[i] = x3;
      }
      x9 = (x4).size();
      x1 = 0;
      x5 = x1;
      for (x6 = 0; x6 < x9; x6++) {
        x7 = (x4[x6]);
        x8 = (x5 + x7);
        x5 = x8;
      }
      return x5;
    } |}]

let%expect_test "" =
  let module C = Code () in
  let open C in
  let f = func "f" (Func (Int, Int)) (fun i -> i + int 1) in
  let g = func "g" (Func (Int, Int)) (fun i -> i - int 1) in
  apply f (apply g (int 0)) |> to_string |> Util.clang_format |> print_endline;
  [%expect
    {|
    #include <set>
    #include <vector>
    int main() {
      int x0;
      int x7;
      int x8;
      int x9;
      x7 = 0;
      x8 = g(x7);
      x9 = f(x8);
      return x9;
    }
    int f(int x1) {
      int x2;
      int x3;
      x2 = 1;
      x3 = (x1 + x2);
      return x3;
    }
    int g(int x4) {
      int x5;
      int x6;
      x5 = 1;
      x6 = (x4 - x5);
      return x6;
    } |}]
