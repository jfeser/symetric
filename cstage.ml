open! Core
module Seq = Sequence

module Code () : Sigs.CODE = struct
  type ctype = Unit | Int | Array of { name : string; elem_type : ctype }

  let rec type_name = function
    | Unit -> "unit"
    | Int -> "int"
    | Array { name; elem_type; _ } ->
        sprintf "%s_%s" (type_name elem_type) name

  type 'a t = { body : string; ret : string; type_ : ctype }

  let append b b' =
    b @ b' |> List.rev
    |> List.fold_left
         ~init:(Set.empty (module String), [])
         ~f:(fun (seen, body) (k, v) ->
           if Set.mem seen k then (seen, body)
           else (Set.add seen k, (k, v) :: body))
    |> fun (_, body) -> body

  let concat bs = bs |> List.reduce ~f:append |> Option.value ~default:[]

  let type_decls = ref []

  let var_decls = ref []

  let fresh =
    let ctr = ref 0 in
    fun () ->
      incr ctr;
      !ctr

  let to_string { body; _ } = body

  let add_type_decl t = type_decls := t :: !type_decls

  let add_var_decl t = var_decls := t :: !var_decls

  let fresh_name () = sprintf "x%d" (fresh ())

  let fresh_var type_ =
    let name = fresh_name () in
    let decl = sprintf "%s %s;" (type_name type_) name in
    add_var_decl decl;
    name

  let elem_type = function Array x -> x.elem_type | _ -> failwith ""

  let rec type_to_str = function
    | Int -> "int"
    | Array t -> sprintf "%s*" (type_to_str t.elem_type)
    | Unit -> failwith "Cannot create value of type unit."

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

  let of_value type_ x =
    let name = fresh_var type_ in
    { ret = name; body = assign name x; type_ }

  let unop fmt type_ x =
    let name = fresh_var type_ in
    { ret = name; body = x.body ^ assign name (sprintf fmt (ret x)); type_ }

  let binop fmt type_ x x' =
    let name = fresh_var type_ in
    {
      ret = name;
      body = x.body ^ x'.body ^ assign name (sprintf fmt (ret x) (ret x'));
      type_;
    }

  let int x = sprintf "%d" x |> of_value Int

  let bool x = (if x then "1" else "0") |> of_value Int

  let unit = of_value Unit ""

  let ( ~- ) = unop "(-%s)" Int

  let ( + ) = binop "(%s + %s)" Int

  let ( - ) = binop "(%s - %s)" Int

  let ( * ) = binop "(%s * %s)" Int

  let ( / ) = binop "(%s / %s)" Int

  let ( mod ) = binop "(%s %% %s)" Int

  let ( = ) = binop "(%s == %s)" Int

  let ( > ) = binop "(%s > %s)" Int

  let ( < ) = binop "(%s < %s)" Int

  let ( <= ) = binop "(%s <= %s)" Int

  let ( >= ) = binop "(%s >= %s)" Int

  let ( && ) = binop "(%s && %s)" Int

  let ( || ) = binop "(%s || %s)" Int

  let not = unop "(!%s)" Int

  let let_ v b =
    let x = b { v with body = "" } in
    { x with body = v.body ^ x.body }

  let ite _ _ _ = failwith ""

  let seq _ _ = failwith ""

  module Array = struct
    let mk_type e =
      let name = sprintf "%s_array" (type_name e) in
      let decl =
        format
          "typedef struct $(name) { int len; ($(elem_type))* elems; } $(name);"
          [ ("name", S name); ("elem_type", S (type_name e)) ]
      in
      add_type_decl decl;
      Array { name; elem_type = e }

    let length x = unop "(%s).len" Int x

    let const t a =
      let a = Array.to_list a in
      let name = fresh_var t in
      let assigns =
        List.mapi a ~f:(fun i x ->
            format {|$(name).elems[$(idx)] = $(val)|}
              [ ("name", S name); ("idx", S (sprintf "%d" i)); ("val", C x) ])
        |> String.concat ~sep:";\n"
      in
      let subst =
        [
          ("name", S name);
          ("type", S "array");
          ("len", S (sprintf "%d" (List.length a)));
          ("assigns", S assigns);
        ]
      in
      let decl = format "$(type) $(name);" subst in
      add_var_decl decl;
      let body =
        format
          {|
$(name).len = $(len);
$(name).elems = malloc(sizeof($(elem_type)) * $(len));
$(assigns)
|}
          subst
      in
      { ret = name; body; type_ = t }

    let init t len f =
      let name = fresh_var t in
      let f_app = f (of_value Int "i") in
      let subst =
        [
          ("name", S name);
          ("type", S (type_name t));
          ("elem_type", S (type_name (elem_type t)));
          ("len", C len);
          ("f_app", C f_app);
        ]
      in
      let decl = format "$(type) $(name);" subst in
      add_var_decl decl;
      let body =
        format
          {|
$(name).len = $(len);
$(name).elems = malloc(sizeof($(elem_type)) * $(len));
for(int i = 0; i < $(len); i++) {
|}
          subst
        ^ format {|$(name).elems[i] = $(f_app);|} subst
        ^ "\n}\n"
      in
      { ret = name; body; type_ = t }

    let set a i x =
      let body =
        format "$(a)[$(i)] = $(x);" [ ("a", C a); ("i", C i); ("x", C x) ]
      in
      { ret = fresh_name (); body; type_ = Unit }

    let get a = binop "(%s[%s])" (elem_type a.type_) a

    let sub a s l = init a.type_ (l - s) (fun i -> get a (s + i))

    let fold a ~init ~f =
      let_ a (fun a ->
          let acc = fresh_var init.type_ in
          let f_app =
            f (of_value init.type_ acc)
              (get a (of_value (elem_type a.type_) "i"))
          in
          let len = length a in
          let subst =
            [
              ("init", C init);
              ("acc", S acc);
              ("len", C len);
              ("f_app", C f_app);
            ]
          in
          let body =
            format
              {|
$(acc) = $(init);
for(int i = 0; i < $(len); i++) {
          |}
              subst
            ^ format {|$(acc) = $(f_app);|} subst
            ^ "}"
          in
          { ret = acc; body; type_ = init.type_ })
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
    x3 = 10;
    x4.len = x3;
    x4.elems = malloc(sizeof(int) * x3);
    for (int i = 0; i < x3; i++) {
      x5 = i;
      x4.elems[i] = x5;
    }
    x11 = (x4).len;
    x2 = 0;
    x6 = x2;
    for (int i = 0; i < x11; i++) {
      x9 = x6;
      x7 = i;
      x8 = (x4[x7]);
      x10 = (x9 + x8);
      x6 = x10;
    } |}]
