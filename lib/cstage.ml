open! Core
module Seq = Sequence

let with_stackmark body =
  let open Delimcc in
  let p = new_prompt () in
  push_prompt p (fun () -> body (fun () -> is_prompt_set p))

module Code () : Sigs.CODE = struct
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

    let input = eformat ~has_effect:false "sexp::load(std::cin)" type_ "" []

    let to_list x =
      eformat "((list*)$(x).get())->get_body()" List.type_ "" [ ("x", C x) ]

    let to_atom x =
      eformat "((atom*)$(x).get())->get_body()" Atom.type_ "" [ ("x", C x) ]
  end

  module Func = struct
    let arg_t = Univ_map.Key.create ~name:"arg_t" [%sexp_of: ctype]

    let ret_t = Univ_map.Key.create ~name:"ret_t" [%sexp_of: ctype]

    let type_ t1 t2 =
      Univ_map.(of_alist_exn Packed.[ T (arg_t, t1); T (ret_t, t2) ])

    let arg_t t = Univ_map.find_exn t arg_t

    let ret_t t = Univ_map.find_exn t ret_t
  end

  let prog =
    let main =
      {
        fname = "main";
        ftype = Func.type_ unit_t unit_t;
        locals = [];
        args = [];
        fbody =
          { ebody = ""; ret = "0"; etype = unit_t; efree = []; eeffect = false };
      }
    in
    { funcs = [ main ]; cur_func = main; fresh = Fresh.create () }

  let add_var_decl x = prog.cur_func.locals <- x :: prog.cur_func.locals

  let fresh_name () = Fresh.name prog.fresh "x%d"

  let fresh_local value =
    let name = fresh_name () in
    let type_ = value.etype in
    eformat name type_ "$(type) $(var) = $(init);"
      [ ("type", S (Type.name type_)); ("var", S name); ("init", C value) ]

  let fresh_global type_ =
    let name = fresh_name () in
    add_var_decl { vname = name; vtype = type_; init = None };
    eformat name type_ "" []

  module C = struct
    type 'a t = expr

    let to_string e = [%sexp_of: expr] e |> Core.Sexp.to_string_hum

    let is_well_scoped c =
      (not c.eeffect) && List.for_all c.efree ~f:(fun (_, m) -> m ())

    let let_ v b =
      let x = fresh_local v in
      let y = b { x with ebody = "" } in
      {
        y with
        ebody = x.ebody ^ y.ebody;
        efree = x.efree @ y.efree;
        eeffect = x.eeffect || y.eeffect;
      }
  end

  include C
  include Genlet.Make (C)

  let sexp_of_t _ = [%sexp_of: expr]

  let free { efree; _ } = efree

  let with_comment s e =
    { e with ebody = sprintf "\n// begin %s\n%s// end %s\n" s e.ebody s }

  let find_func n = List.find prog.funcs ~f:(fun f -> String.(f.fname = n))

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
      find_func "main"
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

  let expr_of_var { vname; vtype; _ } =
    { ebody = ""; ret = vname; etype = vtype; efree = []; eeffect = false }

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

  let int_t = Type.create ~name:"int"

  let bool_t = int_t

  let unit = eformat "0" unit_t "" []

  let rec seq_many = function [] -> unit | x :: xs -> seq x (seq_many xs)

  module Int = struct
    let type_ = int_t

    let int x = eformat (sprintf "%d" x) int_t "" []

    let ( ~- ) x = unop "(-%s)" int_t x

    let ( + ) x y = binop "(%s + %s)" int_t x y

    let ( - ) x y = binop "(%s - %s)" int_t x y

    let ( * ) x y = binop "(%s * %s)" int_t x y

    let ( / ) x y = binop "(%s / %s)" int_t x y

    let ( mod ) x y = binop "(%s %% %s)" int_t x y

    let ( = ) x y = binop "(%s == %s)" bool_t x y

    let ( > ) x y = binop "(%s > %s)" bool_t x y

    let ( < ) x y = binop "(%s < %s)" bool_t x y

    let ( <= ) x y = binop "(%s <= %s)" bool_t x y

    let ( >= ) x y = binop "(%s >= %s)" bool_t x y

    let min x y = binop "std::min(%s, %s)" int_t x y

    let max x y = binop "std::max(%s, %s)" int_t x y

    let of_sexp x =
      eformat "std::stoi(((atom*)$(x).get())->get_body())" int_t ""
        [ ("x", C x) ]
  end

  open Int

  module Bool = struct
    let type_ = bool_t

    let bool x = eformat (if x then "1" else "0") bool_t "" []

    let ( && ) x y = binop "(%s && %s)" bool_t x y

    let ( || ) x y = binop "(%s || %s)" bool_t x y

    let not x = unop "(!%s)" bool_t x

    let of_sexp = Int.of_sexp
  end

  let rec sseq = function [] -> unit | [ x ] -> x | x :: xs -> seq x (sseq xs)

  let print s =
    eformat ~has_effect:true "0" unit_t "std::cout << $(str) << std::endl;"
      [ ("str", S (sprintf "%S" s)) ]

  let exit = eformat ~has_effect:true "0" unit_t "exit(0);" []

  let add_func f = prog.funcs <- f :: prog.funcs

  let func name type_ f =
    let fval =
      { ret = name; ebody = ""; etype = type_; efree = []; eeffect = false }
    in
    if Option.is_none (find_func name) then (
      let arg =
        { vname = fresh_name (); vtype = Func.arg_t type_; init = None }
      in
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
        let ret_type = Func.ret_t func.ftype in
        if Univ_map.mem ret_type is_unit then
          eformat ~has_effect:true "0" ret_type "$(f)($(arg));"
            [ ("f", S func.fname); ("arg", C arg) ]
        else
          eformat ~has_effect:true "$(f)($(arg))" ret_type ""
            [ ("f", S func.fname); ("arg", C arg) ]
    | None -> failwith (sprintf "No function named %s" f.ret)

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

  module Array = struct
    let elem_t = Univ_map.Key.create ~name:"elem_t" [%sexp_of: ctype]

    let mk_type e =
      Type.create ~name:(sprintf "std::vector<%s >" (Type.name e))
      |> Type.add_exn ~key:elem_t ~data:e

    let elem_type t = Univ_map.find_exn t elem_t

    module O = struct
      let ( = ) a a' = binop "(%s == %s)" bool_t a a'
    end

    let length x = unop "((int)((%s).size()))" int_t x

    let const t a =
      let a = Array.to_list a in
      let name = fresh_name () in
      add_var_decl
        { vname = name; vtype = t; init = Some (Int.int (List.length a)) };
      let assigns =
        List.mapi a ~f:(fun i x ->
            eformat "0" unit_t {|$(name)[$(idx)] = $(val);|}
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
      eformat ~has_effect:true "0" unit_t "$(a).clear();" [ ("a", C a) ]

    let reserve a n =
      eformat ~has_effect:true "0" unit_t "$(a).reserve($(n));"
        [ ("a", C a); ("n", C n) ]

    let push_back a x =
      eformat ~has_effect:true "0" unit_t "$(a).push_back($(x));"
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
      |> with_comment "Array.init"

    let set a i x =
      eformat ~has_effect:true "0" unit_t "$(a)[$(i)] = $(x);"
        [ ("a", C a); ("i", C i); ("x", C x) ]

    let get a x =
      eformat "($(a)[$(x)])" (elem_type a.etype) "" [ ("a", C a); ("x", C x) ]

    let map t arr ~f = init t (length arr) (fun i -> let_ (get arr i) f)

    let map2 t a1 a2 ~f = init t (length a1) (fun i -> f (get a1 i) (get a2 i))

    let sub a s l =
      let open Int in
      init a.etype (max (int 0) (l - s)) (fun i -> get a (s + i))

    let fold arr ~init ~f =
      let acc = fresh_global init.etype in
      sseq
        [
          assign init ~to_:acc;
          ( let_ (length arr) @@ fun len ->
            for_ (int 0) (int 1) len (fun i ->
                assign (f acc (get arr i)) ~to_:acc) );
          acc;
        ]
      |> with_comment "Array.fold"

    let iter arr ~f =
      sseq
        [
          ( let_ (length arr) @@ fun len ->
            for_ (int 0) (int 1) len (fun i -> f (get arr i)) );
        ]
      |> with_comment "Array.iter"

    let of_sexp t x elem_of_sexp =
      let_ (Sexp.to_list x) @@ fun l ->
      init t (Sexp.List.length l) (fun i -> elem_of_sexp (Sexp.List.get l i))
  end

  module Set = struct
    let elem_t = Univ_map.Key.create ~name:"elem_t" [%sexp_of: ctype]

    let elem_type t = Univ_map.find_exn t elem_t

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
      eformat "0" unit_t
        {|
for(auto $(iter) = $(set).begin(); $(iter) != $(set).end(); ++$(iter)) {
        $(body)
}
|}
        [ ("set", C a); ("iter", S iter_name); ("body", S body.ebody) ]

    let fold a ~init ~f =
      let_ (fresh_global init.etype) (fun acc ->
          sseq
            [
              assign init ~to_:acc;
              iter a (fun x -> assign (f acc x) ~to_:acc);
              acc;
            ])
      |> with_comment "Set.fold"

    let add a x =
      eformat ~has_effect:true "0" unit_t "$(name).insert($(val));"
        [ ("name", C a); ("val", C x) ]

    let mk_type e =
      Type.create ~name:(sprintf "std::set<%s >" (Type.name e))
      |> Type.add_exn ~key:elem_t ~data:e

    let of_sexp type_ sexp elem_of_sexp =
      let_ (empty type_) @@ fun set ->
      let_ (Sexp.to_list sexp) @@ fun sexp ->
      for_ (int 0) (int 1) (Sexp.List.length sexp) (fun i ->
          add set (elem_of_sexp (Sexp.List.get sexp i)))
  end

  module Tuple = struct
    let fst_t = Univ_map.Key.create ~name:"fst_t" [%sexp_of: ctype]

    let snd_t = Univ_map.Key.create ~name:"snd_t" [%sexp_of: ctype]

    let mk_type x y =
      Type.create
        ~name:(sprintf "std::pair<%s,%s >" (Type.name x) (Type.name y))
      |> Type.add_exn ~key:fst_t ~data:x
      |> Type.add_exn ~key:snd_t ~data:y

    let create x y =
      let type_ = mk_type x.etype y.etype in
      eformat "std::make_pair($(x), $(y))" type_ "" [ ("x", C x); ("y", C y) ]

    let of_sexp x t1_of_sexp t2_of_sexp =
      let_ (Sexp.to_list x) @@ fun x ->
      create
        (t1_of_sexp @@ Sexp.List.get x (int 0))
        (t2_of_sexp @@ Sexp.List.get x (int 1))

    let fst_type t = Univ_map.find_exn t fst_t

    let snd_type t = Univ_map.find_exn t snd_t

    let fst t = eformat "std::get<0>($(t))" (fst_type t.etype) "" [ ("t", C t) ]

    let snd t = eformat "std::get<1>($(t))" (snd_type t.etype) "" [ ("t", C t) ]
  end

  module String = struct
    let type_ = Type.create ~name:"std::string"

    let of_sexp = Sexp.to_atom

    module O = struct
      let ( = ) s s' =
        eformat "($(s)) == ($(s'))" bool_t "" [ ("s", C s); ("s'", C s') ]
    end

    let const s = eformat "\"$(s)\"" type_ "" [ ("s", S (sprintf "%S" s)) ]

    let print s =
      eformat ~has_effect:true "0" unit_t "std::cout << $(str) << std::endl;"
        [ ("str", C s) ]

    let input =
      eformat ~has_effect:true "$(var)" type_
        {|
std::string $(var);
char $(buf)[4096];
while (std::cin.read($(buf), sizeof($(buf)))) {
  $(var).append($(buf), sizeof($(buf)));
}
$(var).append($(buf), std::cin.gcount());
|}
        [ ("var", S (fresh_name ())); ("buf", S (fresh_name ())) ]
  end
end
