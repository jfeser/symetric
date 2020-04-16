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
        | Sphere of (float * float * float * float) C.t
        (*    adt CylinderHint {
	          *    float theta_x;
	          *   float theta_y;
	          *   float theta_z;
	          *   float radius;
	          *   float y;
	          *   float z;
	          *   int xlen;
	          *   float[xlen] xlist;
	          *   int[xlen] xlistint;
         * } *)
        | Cyl of
            ( (float * float * float * float)
            * (float * float * float array * int array) )
            C.t

      let examples_t = C.Array.mk_type C.Bool.type_

      let vectors_t =
        C.Array.mk_type
        @@ C.Tuple_3.mk_type C.Float.type_ C.Float.type_ C.Float.type_

      let spheres_t =
        C.Tuple_4.mk_type C.Float.type_ C.Float.type_ C.Float.type_
          C.Float.type_

      let examples x = Examples x

      let err expected x =
        let got =
          match x with
          | Examples _ -> "examples"
          | Vectors _ -> "vectors"
          | Sphere _ -> "sphere"
        in
        failwith @@ sprintf "Expected %s but got %s" expected got

      let to_examples = function Examples x -> x | x -> err "examples" x

      let to_vectors = function Vectors x -> x | x -> err "vectors" x

      let to_sphere = function Sphere x -> x | x -> err "sphere" x

      let sexp_of _ = assert false

      let of_sexp sym sexp =
        let examples_of_sexp s = C.Array.of_sexp examples_t s C.Bool.of_sexp in
        let sphere_of_sexp s =
          C.Tuple_4.of_sexp s C.Float.of_sexp C.Float.of_sexp C.Float.of_sexp
            C.Float.of_sexp
        in
        let vectors_of_sexp s =
          C.Array.of_sexp vectors_t s @@ fun s ->
          C.Tuple_3.of_sexp s C.Float.of_sexp C.Float.of_sexp C.Float.of_sexp
        in
        if String.(sym = "E") then Examples (examples_of_sexp sexp)
        else if String.(sym = "S") then Sphere (sphere_of_sexp sexp)
        else if String.(sym = "V") then Vectors (vectors_of_sexp sexp)
        else failwith "Unexpected symbol"

      let ( = ) v v' =
        match (v, v') with
        | Examples a, Examples a' -> `Dyn C.Array.O.(a = a')
        | _ -> failwith "Cannot compare"

      let key =
        Univ_map.Key.create ~name:"cad.value" [%sexp_of: [ `E | `V | `S ]]

      let code_of = function
        | Examples x -> C.add_annot (C.cast x) key `E
        | Vectors x -> C.add_annot (C.cast x) key `V
        | Sphere x -> C.add_annot (C.cast x) key `S
        | _ -> failwith "Not convertible"

      let of_code c =
        match C.find_annot c key with
        | Some `E -> Examples (C.cast c)
        | Some `V -> Vectors (C.cast c)
        | Some `S -> Sphere (C.cast c)
        | None -> failwith "Not convertible."
    end

    open Value

    type value = Value.t

    type 'a code = 'a C.t

    let grammar : Grammar.t =
      let open Grammar in
      let open Grammar.Term in
      let nt x = Nonterm x in
      [
        ("E", App ("sphere", [ nt "S"; nt "V" ]));
        ("E", App ("cyl", [ nt "C"; nt "V" ]));
        (* ("E", App ("cuboid", [ nt "CI" ])); *)
        ("E", App ("union", [ nt "E"; nt "E" ]));
        ("E", App ("inter", [ nt "E"; nt "E" ]));
        ("E", App ("sub", [ nt "E"; nt "E" ]));
      ]

    let inverse_rotate (x, y, z) (x', y', z') r =
      (*   float x0 = p.x; float y0 = p.y; float z0 = p.z;
	     * 
	     * float x1 = cos(-theta_z)*x0 - sin(-theta_z)*y0;
	     * float y1 = sin(-theta_z)*x0 + cos(-theta_z)*y0;
	     * float z1 = z0;
	     * 
	     * float x2 = cos(-theta_y)*x1 + sin(-theta_y)*z1;
	     * float y2 = y1;
	     * float z2 = -sin(-theta_y)*x1 + cos(-theta_y)*z1;
	     * 
	     * float x3 = x2;
	     * float y3 = cos(-theta_x)*y2 -sin(-theta_x)*z2;
	     * float z3 = sin(-theta_x)*y2 + cos(-theta_x)*z2;
       * 
	         * return new Vector(x = x3, y = y3, z = z3); *)
      let open C.Float in
      let x0 = x and y0 = y and z0 = z in
      C.let_ ((cos (-z') * x0) - (sin (-z') * y0)) @@ fun x1 ->
      C.let_ ((sin (-z') * x0) + (cos (-z') * y0)) @@ fun y1 ->
      let z1 = z0 in
      C.let_ ((cos (-y') * x1) + (sin (-y') * z1)) @@ fun x2 ->
      let y2 = y1 in
      C.let_ ((-sin (-y') * x1) + (cos (-y') * z1)) @@ fun z2 ->
      let x3 = x2 in
      C.let_ ((cos (-x') * y2) - (sin (-x') * z2)) @@ fun y3 ->
      C.let_ ((sin (-x') * y2) + (cos (-x') * z2)) @@ fun z3 ->
      C.Tuple_3.of_tuple (x3, y3, z3)

    let rec eval ctx = function
      | Grammar.Term.App ("sphere", [ s; v ]) ->
          let sphere = to_sphere (eval ctx s) in
          let vectors = to_vectors (eval ctx v) in
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
      | App (var, []) -> Map.find_exn ctx var
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
    type value = Lang.Value.t

    type 'a code = 'a C.t

    type cache = bool array Sigs.set array

    type t = cache C.t

    let max_size = 10

    open C

    let empty () =
      let type_ = Array.mk_type @@ Set.mk_type @@ Array.mk_type Bool.type_ in
      Nonlocal_let.let_ let_ (fun () ->
          Array.init type_ (Int.int max_size) (fun _ ->
              Set.empty (Array.elem_type type_)))

    let put ~sym:_ ~size tbl v =
      Set.add tbl.(Int.int size) (Lang.Value.to_examples v)

    let iter ~sym ~size ~f tbl =
      if Core.String.(sym = "E") then
        Set.iter tbl.(size) (fun v -> f @@ Lang.Value.examples v)
      else unit

    let print_size _ = failwith "print_size"

    let code_of = Fun.id

    let of_code = Fun.id
  end
end
