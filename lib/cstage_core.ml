open! Core

module type S = sig
  type mark = unit -> bool

  type ctype = Univ_map.t [@@deriving sexp_of]

  and expr = {
    ebody : string;
    ret : string;
    etype : ctype;
    efree : (string * mark) list;
    eeffect : bool;
  }

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

  val type_of : expr -> ctype

  val cast : expr -> expr

  module Type : sig
    val name : ctype -> string

    val create : name:string -> ctype

    val add_exn : ctype -> key:'a Type_equal.Id.t -> data:'a -> ctype
  end

  type fmt_arg = C : expr -> fmt_arg | S : string -> fmt_arg

  val eformat :
    ?has_effect:bool ->
    string ->
    ctype ->
    string ->
    (string * fmt_arg) list ->
    expr

  val unit_t : ctype

  module Func : sig
    val mk_type : ctype -> ctype -> ctype

    val arg_t : ctype -> ctype

    val ret_t : ctype -> ctype

    val func : string -> ctype -> (expr -> expr) -> expr

    (* val of_name : string -> func_decl option *)

    val apply : expr -> expr -> expr
  end

  val add_var_decl : var_decl -> unit

  val fresh_name : unit -> string

  val expr_of_var : var_decl -> expr

  val unit : expr

  val to_string : expr -> string

  val unop :
    (string -> string, unit, string, string, string, string) format6 ->
    ctype ->
    expr ->
    expr

  val binop :
    (string -> string -> string, unit, string, string, string, string) format6 ->
    ctype ->
    expr ->
    expr ->
    expr

  module Sexp : sig
    val type_ : ctype

    module List : sig
      val get : expr -> expr -> expr

      val length : expr -> expr

      val type_ : ctype
    end

    module Atom : sig
      val type_ : ctype
    end

    val input : unit -> expr

    val to_list : expr -> expr

    val to_atom : expr -> expr
  end

  module Bool : sig
    val type_ : ctype

    val bool : bool -> expr

    val ( && ) : expr -> expr -> expr

    val ( || ) : expr -> expr -> expr

    val not : expr -> expr

    val of_sexp : expr -> expr

    val sexp_of : expr -> expr
  end

  val fresh_var : ctype -> mark -> expr

  (* val fresh_local : expr -> expr *)
  val fresh_global : ctype -> expr

  val let_ : expr -> (expr -> expr) -> expr

  val let_global : expr -> (expr -> expr) -> expr

  val genlet : expr -> expr

  val let_locus : (unit -> expr) -> expr

  val with_stackmark : ((unit -> bool) -> 'a) -> 'a

  val ite : expr -> (unit -> expr) -> (unit -> expr) -> expr

  val for_ : expr -> expr -> expr -> (expr -> expr) -> expr

  val seq : expr -> expr -> expr

  val sseq : expr list -> expr

  val assign : expr -> to_:expr -> expr

  val print : string -> expr

  val exit : expr

  val with_comment : string -> expr -> expr
end

module Make () : S = struct
  type mark = unit -> bool

  type ctype = Univ_map.t [@@deriving sexp_of]

  and expr = {
    ebody : string;
    ret : string;
    etype : ctype;
    efree : (string * (mark[@opaque])) list;
    eeffect : bool;
  }
  [@@deriving sexp_of]

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

  type fmt_arg = C : expr -> fmt_arg | S : string -> fmt_arg

  let type_of e = e.etype

  let cast = Fun.id

  module Type = struct
    let name_k = Univ_map.Key.create ~name:"name" [%sexp_of: string]

    let name t = Univ_map.find_exn t name_k

    let create ~name = Univ_map.(of_alist_exn Packed.[ T (name_k, name) ])

    let add_exn t ~key ~data = Univ_map.add_exn t key data
  end

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
    and ret = format ret_fmt args
    and efree =
      List.concat_map args ~f:(function _, C { efree; _ } -> efree | _ -> [])
    and eeffect =
      List.exists args ~f:(function
        | _, C { eeffect; _ } -> eeffect
        | _ -> false)
    in
    { ebody; ret; etype; efree; eeffect = has_effect || eeffect }

  let is_unit = Univ_map.Key.create ~name:"is_unit" [%sexp_of: unit]

  let unit_t = Type.create ~name:"int" |> Type.add_exn ~key:is_unit ~data:()

  let int_t = Type.create ~name:"int"

  let arg_t = Univ_map.Key.create ~name:"arg_t" [%sexp_of: ctype]

  let ret_t = Univ_map.Key.create ~name:"ret_t" [%sexp_of: ctype]

  let func_t t1 t2 =
    Univ_map.(of_alist_exn Packed.[ T (arg_t, t1); T (ret_t, t2) ])

  let prog =
    let main =
      {
        fname = "main";
        ftype = func_t unit_t unit_t;
        locals = [];
        args = [];
        fbody =
          { ebody = ""; ret = "0"; etype = unit_t; efree = []; eeffect = false };
      }
    in
    { funcs = [ main ]; cur_func = main; fresh = Fresh.create () }

  let add_var_decl x = prog.cur_func.locals <- x :: prog.cur_func.locals

  let add_func f = prog.funcs <- f :: prog.funcs

  let fresh_name () = Fresh.name prog.fresh "x%d"

  let expr_of_var { vname; vtype; _ } =
    { ebody = ""; ret = vname; etype = vtype; efree = []; eeffect = false }

  let unit = eformat "0" unit_t "" []

  module Func = struct
    let mk_type = func_t

    let arg_t t = Univ_map.find_exn t arg_t

    let ret_t t = Univ_map.find_exn t ret_t

    let of_name n = List.find prog.funcs ~f:(fun f -> String.(f.fname = n))

    let func name type_ f =
      let fval =
        { ret = name; ebody = ""; etype = type_; efree = []; eeffect = false }
      in
      if Option.is_none (of_name name) then (
        let arg = { vname = fresh_name (); vtype = arg_t type_; init = None } in
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
      match of_name f.ret with
      | Some func ->
          let ret_type = ret_t func.ftype in
          if Univ_map.mem ret_type is_unit then
            eformat ~has_effect:true "0" ret_type "$(f)($(arg));"
              [ ("f", S func.fname); ("arg", C arg) ]
          else
            eformat ~has_effect:true "$(f)($(arg))" ret_type ""
              [ ("f", S func.fname); ("arg", C arg) ]
      | None -> failwith (sprintf "No function named %s" f.ret)
  end

  let to_string e =
    let expr_to_str e = e.ebody
    and var_decl_to_str v =
      let subst = [ ("type", S (Type.name v.vtype)); ("name", S v.vname) ] in
      match v.init with
      | Some init ->
          format "$(type) $(name) ($(init));" (("init", C init) :: subst)
      | None -> format "$(type) $(name);" subst
    in

    let arg_to_str v = sprintf "const %s &%s" (Type.name v.vtype) v.vname in
    let func_to_decl_str f =
      let args_str = List.map f.args ~f:arg_to_str |> String.concat ~sep:"," in
      sprintf "%s %s(%s);" (Type.name (Func.ret_t f.ftype)) f.fname args_str
    and func_to_def_str f =
      let locals_str =
        List.rev f.locals
        |> List.map ~f:var_decl_to_str
        |> String.concat ~sep:" "
      and args_str = List.map f.args ~f:arg_to_str |> String.concat ~sep:","
      and ret_t_name = Type.name (Func.ret_t f.ftype) in
      if String.(f.fname = "main") then
        sprintf "%s %s(%s) { %s return %s; }" ret_t_name f.fname args_str
          (expr_to_str f.fbody) f.fbody.ret
      else
        sprintf "%s %s(%s) { %s %s return %s; }" ret_t_name f.fname args_str
          locals_str (expr_to_str f.fbody) f.fbody.ret
    in

    prog.cur_func.fbody <-
      { e with ebody = prog.cur_func.fbody.ebody ^ e.ebody };

    let header =
      {|
#include <vector>
#include <set>
#include <iostream>

#include "sexp.hpp"
|}
    and forward_decls =
      List.map prog.funcs ~f:func_to_decl_str |> String.concat ~sep:"\n"
    and main_decls =
      Func.of_name "main"
      |> Option.map ~f:(fun func ->
             List.rev func.locals
             |> List.map ~f:var_decl_to_str
             |> String.concat ~sep:" ")
      |> Option.value ~default:""
    and funcs =
      List.rev prog.funcs
      |> List.map ~f:func_to_def_str
      |> String.concat ~sep:"\n"
    in
    header ^ forward_decls ^ main_decls ^ funcs

  let unop fmt type_ x =
    eformat (sprintf fmt "$(arg)") type_ "" [ ("arg", C x) ]

  let binop fmt type_ x x' =
    eformat
      (sprintf fmt "$(arg1)" "$(arg2)")
      type_ ""
      [ ("arg1", C x); ("arg2", C x') ]

  module Sexp = struct
    let type_ = Type.create ~name:"const std::unique_ptr<sexp>&"

    module List = struct
      let get x i = eformat "($(x))[$(i)]" type_ "" [ ("x", C x); ("i", C i) ]

      let length x =
        eformat "(int)(($(x)).size())" (Type.create ~name:"int") ""
          [ ("x", C x) ]

      let type_ = Type.create ~name:"const std::vector<std::unique_ptr<sexp>>&"
    end

    module Atom = struct
      let type_ = Type.create ~name:"std::string"
    end

    let input () =
      eformat ~has_effect:false "$(name)" type_
        "std::unique_ptr<sexp> $(name) = sexp::load(std::cin);"
        [ ("name", S (fresh_name ())) ]

    let to_list x =
      eformat "((list*)$(x).get())->get_body()" List.type_ "" [ ("x", C x) ]

    let to_atom x =
      eformat "((atom*)$(x).get())->get_body()" Atom.type_ "" [ ("x", C x) ]
  end

  module Bool = struct
    let type_ = Type.create ~name:"int"

    let bool x = eformat (if x then "1" else "0") type_ "" []

    let ( && ) x y = binop "(%s && %s)" type_ x y

    let ( || ) x y = binop "(%s || %s)" type_ x y

    let not x = unop "(!%s)" type_ x

    let of_sexp x =
      eformat "std::stoi(((atom*)$(x).get())->get_body())" type_ ""
        [ ("x", C x) ]

    let sexp_of x =
      eformat "atom(std::to_string($(x)))" Sexp.type_ "" [ ("x", C x) ]
  end

  let fresh_var etype m =
    let name = Fresh.name prog.fresh "x%d" in
    { ret = name; etype; efree = [ (name, m) ]; ebody = ""; eeffect = false }

  let fresh_local value =
    let name = fresh_name () in
    let type_ = value.etype in
    eformat name type_ "$(type) $(var) = $(init);"
      [ ("type", S (Type.name type_)); ("var", S name); ("init", C value) ]

  let fresh_global type_ =
    let name = fresh_name () in
    add_var_decl { vname = name; vtype = type_; init = None };
    eformat name type_ "" []

  let let_ v b =
    let x = fresh_local v in
    let y = b { x with ebody = "" } in
    {
      y with
      ebody = x.ebody ^ y.ebody;
      efree = x.efree @ y.efree;
      eeffect = x.eeffect || y.eeffect;
    }

  include Genlet.Make (struct
    type 'a t = expr

    let let_ = let_

    let to_string e = [%sexp_of: expr] e |> Core.Sexp.to_string_hum

    let is_well_scoped c =
      (not c.eeffect) && List.for_all c.efree ~f:(fun (_, m) -> m ())
  end)

  let ite cond then_ else_ =
    let_locus @@ fun () ->
    let then_ = then_ () in
    let else_ = else_ () in
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
          let i = fresh_var int_t m in
          let body = let_locus @@ fun () -> f i in
          (i, body))
    in
    eformat "0" unit_t
      {|
for(int $(i) = $(lo); $(i) < $(hi); $(i) += $(step)) {
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

  let seq e e' =
    {
      ebody = e.ebody ^ e'.ebody;
      etype = e'.etype;
      ret = e'.ret;
      efree = e.efree @ e'.efree;
      eeffect = e.eeffect || e'.eeffect;
    }

  let rec sseq = function [] -> unit | [ x ] -> x | x :: xs -> seq x (sseq xs)

  let assign x ~to_:v =
    eformat ~has_effect:true "0" unit_t {|$(v) = $(x);|}
      [ ("x", C x); ("v", C v) ]

  let let_global v b =
    let g = fresh_global v.etype in
    let x = seq (assign v ~to_:g) g in
    let y = b { x with ebody = "" } in
    {
      y with
      ebody = x.ebody ^ y.ebody;
      efree = x.efree @ y.efree;
      eeffect = x.eeffect || y.eeffect;
    }

  let print s =
    eformat ~has_effect:true "0" unit_t "std::cout << $(str) << std::endl;"
      [ ("str", S (sprintf "%S" s)) ]

  let exit = eformat ~has_effect:true "0" unit_t "exit(0);" []

  let with_comment s e =
    { e with ebody = sprintf "\n// begin %s\n%s// end %s\n" s e.ebody s }
end
