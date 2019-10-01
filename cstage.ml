open! Core
module Seq = Sequence

module Code () : Sigs.CODE = struct
  type ctype = Unit | Int | Array of ctype

  let elem_type = function Array x -> x | _ -> failwith ""

  type 'a t = {
    decls : string list;
    body : string list;
    ret : string;
    type_ : ctype;
  }

  let fresh =
    let ctr = ref 0 in
    fun () ->
      incr ctr;
      !ctr

  let rec type_to_str = function
    | Int -> "int"
    | Array t -> sprintf "%s*" (type_to_str t)
    | Unit -> failwith "Cannot create value of type unit."

  let name () = sprintf "x%d" (fresh ())

  let decl ctx name type_ =
    { ctx with decls = sprintf "%s %s;" type_ name :: ctx.decls }

  let unop fmt type_ x = { x with ret = sprintf fmt x.ret; type_ }

  let binop fmt type_ x x' =
    {
      ret = sprintf fmt x.ret x'.ret;
      decls = x.decls @ x'.decls;
      body = x.body @ x'.body;
      type_;
    }

  let of_value t v = { decls = []; body = []; ret = v; type_ = t }

  let int x = sprintf "%d" x |> of_value Int

  let bool x = (if x then "1" else "0") |> of_value Int

  let unit = of_value Unit ""

  let array f a =
    let elems = Array.map a ~f in
    let decls = Array.fold elems ~init:[] ~f:(fun xs x -> x.decls @ xs) in
    let elem_type = elems.(0).type_ in
    let name = name () in
    let type_ = Array elem_type in
    let decl =
      let elems_str =
        Array.to_list elems
        |> List.map ~f:(fun x -> x.ret)
        |> String.concat ~sep:", "
      in
      sprintf "%s %s[] = { %s };" (type_to_str type_) name elems_str
    in
    { decls = decl :: decls; body = []; ret = name; type_ }

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

  let init _ _ = failwith ""

  let get a = binop "(%s[%s])" (elem_type a.type_) a

  let set _ = failwith ""

  let sub _ _ _ = failwith ""

  let fold _ ~init:_ ~f:_ = failwith ""

  let let_ _ _ = failwith ""

  let ite _ _ _ = failwith ""

  let seq _ _ = failwith ""
end
