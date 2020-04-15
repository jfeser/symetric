module type Deps = sig
  include Sigs.CODE

  module Array :
    Cstage_array.S
      with type 'a t := 'a t
       and type 'a ctype := 'a ctype
       and type sexp := Sigs.sexp

  module Float :
    Cstage_float.S
      with type 'a t := 'a t
       and type 'a ctype := 'a ctype
       and type sexp := Sigs.sexp

  module Tuple_3 :
    Cstage_tuple.Tuple_3.S
      with type 'a t := 'a t
       and type 'a ctype := 'a ctype
       and type sexp := Sigs.sexp

  module Tuple_4 :
    Cstage_tuple.Tuple_4.S
      with type 'a t := 'a t
       and type 'a ctype := 'a ctype
       and type sexp := Sigs.sexp
end

module Make (C : Deps) = struct
  module Lang = struct
    module Value = struct
      type value = Value

      type t =
        | Examples of bool array C.t
        | Vectors of (float * float * float) array C.t
        | Spheres of (float * float * float * float) array C.t
        | Int of int32 C.t

      let examples_t = C.Array.mk_type C.Bool.type_

      let vectors_t =
        C.Array.mk_type
        @@ C.Tuple_3.mk_type C.Float.type_ C.Float.type_ C.Float.type_

      let examples x = Examples x

      let to_examples = function
        | Examples x -> x
        | _ -> failwith "Expected examples"

      let to_vectors = function
        | Vectors x -> x
        | _ -> failwith "Expected vectors"

      let to_spheres = function
        | Spheres x -> x
        | _ -> failwith "Expected spheres"

      let to_int = function Int x -> x | _ -> failwith "Expected int"

      let sexp_of _ = assert false

      let of_sexp _ = assert false

      let ( = ) _ _ = assert false

      let of_code _ = assert false

      let code_of _ = assert false
    end

    open Value

    type value = Value.t

    type 'a code = 'a C.t

    let grammar : Grammar.t =
      let open Grammar in
      let open Grammar.Term in
      let nt x = Nonterm x in
      [
        ("E", App ("sphere", [ nt "S"; nt "SI"; nt "V" ]));
        ("E", App ("cyl", [ nt "YI" ]));
        ("E", App ("cuboid", [ nt "CI" ]));
        ("E", App ("union", [ nt "E"; nt "E" ]));
        ("E", App ("inter", [ nt "E"; nt "E" ]));
        ("E", App ("sub", [ nt "E"; nt "E" ]));
      ]

    let rec eval ctx = function
      | Grammar.Term.App ("sphere", [ i ]) ->
          let sphere =
            C.Array.get
              (Map.find_exn ctx "spheres" |> to_spheres)
              (eval ctx i |> to_int)
          in
          let vectors = Map.find_exn ctx "vectors" |> to_vectors in
          let x, y, z, r = C.Tuple_4.tuple_of sphere in
          examples
          @@ C.Array.map examples_t vectors ~f:(fun v ->
                 let x', y', z' = C.Tuple_3.tuple_of v in
                 C.Float.(
                   ((x - x') ** float 2.0)
                   + ((y - y') ** float 2.0)
                   + ((z - z') ** float 2.0)
                   < r ** float 2.0))
      | App ("inter", [ e1; e2 ]) ->
          let v1 = eval ctx e1 |> to_examples
          and v2 = eval ctx e2 |> to_examples in
          examples @@ C.Array.map2 examples_t v1 v2 ~f:C.Bool.( && )
      | App ("union", [ e1; e2 ]) ->
          let v1 = eval ctx e1 |> to_examples
          and v2 = eval ctx e2 |> to_examples in
          examples @@ C.Array.map2 examples_t v1 v2 ~f:C.Bool.( || )
      | App ("sub", [ e1; e2 ]) ->
          let v1 = eval ctx e1 |> to_examples
          and v2 = eval ctx e2 |> to_examples in
          examples
          @@ C.Array.map2 examples_t v1 v2 ~f:C.Bool.(fun x1 x2 -> x1 && not x2)
      | e ->
          Error.create "Unexpected expression." e [%sexp_of: Grammar.Term.t]
          |> Error.raise

    let eval ctx expr =
      try eval ctx expr
      with exn ->
        let open Error in
        let err = of_exn exn in
        tag_arg err "Evaluation failed" expr [%sexp_of: Grammar.Term.t] |> raise
  end

  module Cache = struct
    type cache = Cache

    type value = Lang.Value.t

    type t = bool array Sigs.set C.t
  end
end
