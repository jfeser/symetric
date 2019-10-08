open! Core
module Seq = Sequence

module Code () : Sigs.CODE = struct
  type 'a set = Set

  type 'a ntype = { name : string; elem_type : 'a ctype }

  and 'a ctype =
    | Unit : unit ctype
    | Int : int ctype
    | Bool : bool ctype
    | Array : 'a ntype -> 'a array ctype
    | Set : 'a ntype -> 'a set ctype
    | Tuple : string * 'a ctype * 'b ctype -> ('a * 'b) ctype
    | Func : 'a ctype * 'b ctype -> ('a -> 'b) ctype

  let type_name (type a) (ctype : a ctype) =
    match ctype with
    | Unit -> "int"
    | Int -> "int"
    | Bool -> "int"
    | Tuple (n, _, _) -> n
    | Array { name; _ } | Set { name; _ } -> name
    | _ -> failwith "Type cannot be constructed."

  type 'a t = { body : string; ret : string; type_ : 'a ctype }

  type func_decl =
    | FuncDecl : ('a -> 'b) ctype * ('a -> 'b) t * string -> func_decl

  let var_decls = ref []

  let func_decls = ref []

  let fresh =
    let ctr = ref 0 in
    fun () ->
      incr ctr;
      !ctr

  let to_string { body; _ } =
    let template =
      format_of_string
        {|
#include <vector>
#include <set>
using namespace std;

%s

int main() {
      %s

      %s
}
|}
    in
    sprintf template
      (String.concat ~sep:"\n"
         ( List.rev !func_decls
         |> List.map ~f:(fun (_, FuncDecl (_, _, decl)) -> decl) ))
      (String.concat ~sep:"\n" (List.rev !var_decls))
      body

  let add_var_decl t = var_decls := t :: !var_decls

  let fresh_name () = sprintf "x%d" (fresh ())

  let fresh_var (type a) (type_ : a ctype) =
    let name = fresh_name () in
    let decl = sprintf "%s %s;" (type_name type_) name in
    add_var_decl decl;
    { ret = name; body = ""; type_ }

  let ret { ret; _ } = ret

  let body { body; _ } = body

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
                  String.substr_replace_all fmt ~pattern:pat ~with_:(ret v)
                in
                body v ^ fmt
              else fmt)
    in
    ( if String.contains fmt '$' then
      Error.(create "Incomplete template." fmt [%sexp_of: string] |> raise) );
    fmt

  let assign = sprintf "%s = %s;"

  let of_value (type a) (type_ : a ctype) x =
    let var_ = fresh_var type_ in
    { var_ with body = var_.body ^ assign var_.ret x }

  let let_ v b =
    let x = b { v with body = "" } in
    { x with body = v.body ^ x.body }

  let unop fmt type_ x =
    let var_ = fresh_var type_ in
    {
      var_ with
      body = var_.body ^ x.body ^ assign var_.ret (sprintf fmt (ret x));
    }

  let binop fmt type_ x x' =
    let var_ = fresh_var type_ in
    let_ x (fun x ->
        let_ x' (fun x' ->
            {
              var_ with
              body =
                var_.body ^ x.body ^ x'.body
                ^ assign var_.ret (sprintf fmt (ret x) (ret x'));
            }))

  let int x : int t = sprintf "%d" x |> of_value Int

  let bool x : bool t = (if x then "1" else "0") |> of_value Bool

  let unit = of_value Unit "0"

  let ( ~- ) = unop "(-%s)" Int

  let ( + ) = binop "(%s + %s)" Int

  let ( - ) = binop "(%s - %s)" Int

  let ( * ) = binop "(%s * %s)" Int

  let ( / ) = binop "(%s / %s)" Int

  let ( mod ) = binop "(%s %% %s)" Int

  let ( = ) = binop "(%s == %s)" Bool

  let ( > ) = binop "(%s > %s)" Bool

  let ( < ) = binop "(%s < %s)" Bool

  let ( <= ) = binop "(%s <= %s)" Bool

  let ( >= ) = binop "(%s >= %s)" Bool

  let ( && ) = binop "(%s && %s)" Bool

  let ( || ) = binop "(%s || %s)" Bool

  let not = unop "(!%s)" Bool

  let ite cond then_ else_ =
    let ret_var = fresh_var then_.type_ in
    {
      ret_var with
      body =
        format "if ($(cond)) { $(ret) = $(then); } else { $(ret) = $(else); }"
          [
            ("ret", C ret_var);
            ("cond", C cond);
            ("then", C then_);
            ("else", C else_);
          ];
    }

  let seq e e' = { e' with body = e.body ^ e'.body }

  let print s = { unit with body = sprintf "std::cout << %S << endl;" s }

  let func (type a b) name (type_ : (a -> b) ctype) (f : a t -> b t) =
    let (Func (in_type, ret_type)) = type_ in
    match List.Assoc.find ~equal:String.( = ) !func_decls name with
    | Some (FuncDecl (_, val_, _)) -> Obj.magic val_
    | None ->
        let arg = { ret = fresh_name (); body = ""; type_ = in_type } in
        let val_ = { ret = name; body = ""; type_ } in
        func_decls :=
          List.Assoc.add ~equal:String.( = ) !func_decls name
            (FuncDecl (type_, val_, ""));
        let decl =
          let args =
            [
              ("rtype", S (type_name ret_type));
              ("name", S name);
              ("itype", S (type_name in_type));
              ("arg", S arg.ret);
              ("body", C (f arg));
            ]
          in
          format "$(rtype) $(name)($(itype) $(arg)) {" args
          ^ format "return $(body); }" args
        in
        func_decls :=
          List.Assoc.add ~equal:String.( = ) !func_decls name
            (FuncDecl (type_, val_, decl));
        val_

  let apply f arg =
    let (Func (_, type_)) = f.type_ in
    let var_ = fresh_var type_ in
    {
      var_ with
      body =
        format "$(var) = $(f)($(arg))"
          [ ("var", S var_.ret); ("f", S f.ret); ("arg", C arg) ];
    }

  module Array = struct
    let mk_type e =
      let name = sprintf "std::vector<%s>" (type_name e) in
      Array { name; elem_type = e }

    let elem_type = function Array { elem_type; _ } -> elem_type | _ -> .

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
      let subst =
        [
          ("name", S name);
          ("type", S (type_name t));
          ("len", S (sprintf "%d" (List.length a)));
          ("assigns", S assigns);
        ]
      in
      let decl = format "$(type) $(name) ($(len));" subst in
      add_var_decl decl;
      { ret = name; body = assigns; type_ = t }

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
      let decl = format "$(type) $(name) ($(len));" subst in
      add_var_decl decl;
      let body =
        format "for(int i = 0; i < $(len); i++) {\n" subst
        ^ format "$(name)[i] = $(f_app);\n" subst
        ^ "}\n"
      in
      { ret = name; body; type_ = t }

    let set a i x =
      let body =
        format "$(a)[$(i)] = $(x);" [ ("a", C a); ("i", C i); ("x", C x) ]
      in
      { unit with body }

    let get a = binop "(%s[%s])" (elem_type a.type_) a

    let sub a s l = init a.type_ (l - s) (fun i -> get a (s + i))

    let fold a ~init ~f =
      let acc = fresh_var init.type_ in
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
          let body =
            format
              {| $(acc) = $(init); for($(iter) = 0; $(iter) < $(len); $(iter)++) { |}
              subst
            ^ format {|$(acc) = $(f_app);|} subst
            ^ "}"
          in
          { acc with body })
  end

  module Set = struct
    let mk_type e =
      let name = sprintf "std::set<%s>" (type_name e) in
      Set { name; elem_type = e }

    let elem_type = function Set { elem_type; _ } -> elem_type | _ -> .

    let empty ctype =
      let set = fresh_name () in
      let decl =
        format "$(type) $(name);"
          [ ("type", S (type_name ctype)); ("name", S set) ]
      in
      add_var_decl decl;
      { ret = set; body = ""; type_ = ctype }

    let iter a f =
      let iter = fresh_name () in
      let_ a (fun a ->
          let f_app = f (of_value (elem_type a.type_) (sprintf "*%s" iter)) in
          let subst =
            [ ("name", C a); ("f_app", C f_app); ("iter", S iter) ]
          in
          let body =
            format
              "for(auto $(iter) = $(name).begin(); $(iter) != $(name).end(); \
               ++$(iter)) {"
              subst
            ^ format {|$(f_app);|} subst ^ "}"
          in
          { unit with body })

    let add a x =
      let_ a (fun a ->
          let_ x (fun x ->
              {
                unit with
                body =
                  format "$(name).insert($(val));"
                    [ ("name", C a); ("val", C x) ];
              }))
  end

  module Tuple = struct
    let mk_type x y =
      let name = sprintf "std::pair<%s,%s>" (type_name x) (type_name y) in
      Tuple (name, x, y)

    let fst_type = function Tuple (_, x, _) -> x | _ -> .

    let snd_type = function Tuple (_, _, x) -> x | _ -> .

    let create x y =
      let type_ = mk_type x.type_ y.type_ in
      let var_ = fresh_var type_ in
      let body =
        format "$(var) = std::make_pair($(x), $(y));"
          [ ("var", C var_); ("x", C x); ("y", C y) ]
      in
      { var_ with body }

    let fst t =
      let type_ = fst_type t.type_ in
      let var_ = fresh_var type_ in
      let body =
        format "$(var) = std::get<0>($(t));" [ ("var", C var_); ("t", C t) ]
      in
      { var_ with body }

    let snd t =
      let type_ = snd_type t.type_ in
      let var_ = fresh_var type_ in
      let body =
        format "$(var) = std::get<1>($(t));" [ ("var", C var_); ("t", C t) ]
      in
      { var_ with body }
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
    #include <vector>
    using namespace std;

    int main() {
      int x2;
      int x3;
      int x5;
      x3 = 10;
      std::vector<int> x4(x3);
      int x6;
      int x7;
      int x8;
      int x9;
      int x10;
      int x11;

      x3 = 10;
      for (int i = 0; i < x3; i++) {
        x5 = i;
        x4[i] = x5;
      }
      x11 = (x4).size();
      x2 = 0;
      x6 = x2;
      for (int i = 0; i < x11; i++) {
        x9 = x6;
        x7 = i;
        x8 = (x4[x7]);
        x10 = (x9 + x8);
        x6 = x10;
      }
    } |}]
