open Types

module type Deps = sig
  include Sigs.CODE

  module Array :
    Cstage_array.S with type 'a code := 'a t and type 'a ctype := 'a ctype

  module Float :
    Cstage_float.S with type 'a code := 'a t and type 'a ctype := 'a ctype

  module Tuple_3 :
    Cstage_tuple.Tuple_3.S
      with type 'a code := 'a t
       and type 'a ctype := 'a ctype

  module Tuple_4 :
    Cstage_tuple.Tuple_4.S
      with type 'a code := 'a t
       and type 'a ctype := 'a ctype
end

module Make (C : Deps) = struct
  module Lang = struct
    module Value = struct
      type value = Value

      type offset = (C.Int.t * C.Float.t) C.t

      type vectors_t =
        (C.Float.t (* x *), C.Float.t (* y *), C.Float.t (* z *)) C.Tuple_3.t
        C.Array.t

      type sphere_t =
        ( C.Float.t (* x *),
          C.Float.t (* y *),
          C.Float.t (* z *),
          C.Float.t (* r *) )
        C.Tuple_4.t

      type cylinder_t =
        ( int (* id *),
          ( C.Float.t (* theta_x *),
            C.Float.t (* theta_y *),
            C.Float.t (* theta_z *),
            C.Float.t (* radius *) )
          C.Tuple_4.t,
          C.Float.t (* y *) * C.Float.t
        (* z *) )
        C.Tuple_3.t

      type t =
        | Examples of bool C.Array.t C.t
        | Vectors of vectors_t C.t
        | Sphere of sphere_t C.t
        | Cylinder of cylinder_t C.t
        | Cylinder_offset of offset
        | Cuboid of
            ( int (* id *),
              C.Float.t (* theta_x *),
              C.Float.t (* theta_y *),
              C.Float.t (* theta_z *) )
            C.Tuple_4.t
        | Cuboid_x_offset of offset
        | Cuboid_y_offset of offset
        | Cuboid_z_offset of offset
        | Bool of bool C.t
        | Int of int C.t

      let pp fmt = function
        | Examples _ -> Fmt.pf fmt "examples"
        | Vectors _ -> Fmt.pf fmt "vectors"
        | Sphere _ -> Fmt.pf fmt "sphere"
        | Cylinder _ -> Fmt.pf fmt "cylinder"
        | Cylinder_offset _ -> Fmt.pf fmt "cylinder-offset"
        | Bool _ -> Fmt.pf fmt "bool"
        | Int _ -> Fmt.pf fmt "int"
        | Cuboid _ -> Fmt.pf fmt "cuboid"
        | Cuboid_x_offset _ -> Fmt.pf fmt "cuboid-x-offset"
        | Cuboid_y_offset _ -> Fmt.pf fmt "cuboid-y-offset"
        | Cuboid_z_offset _ -> Fmt.pf fmt "cuboid-z-offset"

      let examples_t = C.Array.mk_type C.Bool.type_

      let offset_t = C.Tuple.mk_type C.Int.type_ C.Float.type_

      let vectors_t : vectors_t C.ctype =
        C.Array.mk_type
        @@ C.Tuple_3.mk_type C.Float.type_ C.Float.type_ C.Float.type_

      let sphere_t : sphere_t C.ctype =
        C.Tuple_4.mk_type C.Float.type_ C.Float.type_ C.Float.type_
          C.Float.type_

      let cylinder_t : cylinder_t C.ctype =
        C.Tuple_3.mk_type C.Int.type_
          (C.Tuple_4.mk_type C.Float.type_ C.Float.type_ C.Float.type_
             C.Float.type_)
          (C.Tuple.mk_type C.Float.type_ C.Float.type_)

      let examples x = Examples x

      let err expected x =
        let got = Fmt.str "%a" pp x in
        failwith @@ sprintf "Expected %s but got %s" expected got

      let to_examples = function Examples x -> x | x -> err "examples" x

      let to_vectors = function Vectors x -> x | x -> err "vectors" x

      let to_sphere = function Sphere x -> x | x -> err "sphere" x

      let to_cylinder = function Cylinder x -> x | x -> err "cylinder" x

      let to_bool = function Bool x -> x | x -> err "bool" x

      let to_int = function Int x -> x | x -> err "int" x

      let to_offset = function
        | Cylinder_offset x
        | Cuboid_x_offset x
        | Cuboid_y_offset x
        | Cuboid_z_offset x ->
            x
        | x -> err "offset" x

      let sexp_of _ = assert false

      let of_sexp sym sexp =
        let examples_of_sexp s = C.Array.of_sexp s C.Bool.of_sexp in
        let sphere_of_sexp s =
          C.Tuple_4.of_sexp s C.Float.of_sexp C.Float.of_sexp C.Float.of_sexp
            C.Float.of_sexp
        in
        let cylinder_of_sexp s =
          C.Tuple_3.of_sexp s C.Int.of_sexp
            (fun s ->
              C.Tuple_4.of_sexp s C.Float.of_sexp C.Float.of_sexp
                C.Float.of_sexp C.Float.of_sexp)
            (fun s -> C.Tuple.of_sexp s C.Float.of_sexp C.Float.of_sexp)
        in
        let vectors_of_sexp s =
          C.Array.of_sexp s @@ fun s ->
          C.Tuple_3.of_sexp s C.Float.of_sexp C.Float.of_sexp C.Float.of_sexp
        in
        let offset_of_sexp s =
          C.Tuple.of_sexp s C.Int.of_sexp C.Float.of_sexp
        in
        if String.(sym = "E") then Examples (examples_of_sexp sexp)
        else if String.(sym = "S") then Sphere (sphere_of_sexp sexp)
        else if String.(sym = "V") then Vectors (vectors_of_sexp sexp)
        else if String.(sym = "C") then Cylinder (cylinder_of_sexp sexp)
        else if String.(sym = "CO") then Cylinder_offset (offset_of_sexp sexp)
        else failwith "Unexpected symbol"

      let ( = ) v v' =
        match (v, v') with
        | Examples a, Examples a' -> `Dyn C.Array.O.(a = a')
        | Int x, Int x' -> `Dyn C.Int.(x = x')
        | _ -> failwith "Cannot compare"

      let key =
        Univ_map.Key.create ~name:"cad.value"
          [%sexp_of: [ `E | `V | `S | `I | `C | `CO ]]

      let code_of = function
        | Examples x -> C.add_annot (C.cast x) key `E
        | Vectors x -> C.add_annot (C.cast x) key `V
        | Sphere x -> C.add_annot (C.cast x) key `S
        | Int x -> C.add_annot (C.cast x) key `I
        | Cylinder x -> C.add_annot (C.cast x) key `C
        | Cylinder_offset x -> C.add_annot (C.cast x) key `CO
        | _ -> failwith "Not convertible"

      let of_code c =
        match C.find_annot c key with
        | Some `E -> Examples (C.cast c)
        | Some `V -> Vectors (C.cast c)
        | Some `S -> Sphere (C.cast c)
        | Some `I -> Int (C.cast c)
        | Some `C -> Cylinder (C.cast c)
        | Some `CO -> Cylinder_offset (C.cast c)
        | None -> failwith "Not convertible."
    end

    open Value

    type value = Value.t

    type 'a code = 'a C.t

    let fresh = Fresh.create ()

    let inverse_rotate (x, y, z) (x', y', z') =
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
      | Grammar.As (t, _) -> eval ctx t
      | App ("sphere", [ s; v ]) ->
          let sphere = to_sphere (eval ctx s) in
          let vectors = to_vectors (eval ctx v) in
          examples @@ C.Tuple_4.tuple_of sphere
          @@ fun (x, y, z, r) ->
          C.Array.map vectors ~f:(fun v ->
              C.Tuple_3.tuple_of v @@ fun (x', y', z') ->
              C.Float.(
                ((x - x') ** float 2.0)
                + ((y - y') ** float 2.0)
                + ((z - z') ** float 2.0)
                < r ** float 2.0))
      | App ("cyl", [ c; lo; hi; v ]) ->
          let cyl = to_cylinder @@ eval ctx c
          and lo = to_offset @@ eval ctx lo |> C.Tuple.snd
          and hi = to_offset @@ eval ctx hi |> C.Tuple.snd
          and vectors = to_vectors @@ eval ctx v in
          examples @@ C.Tuple_3.tuple_of cyl
          @@ fun (_, disc, center) ->
          C.Tuple_4.tuple_of disc @@ fun (theta_x, theta_y, theta_z, radius) ->
          C.let_ (C.Tuple.fst center) @@ fun c_y ->
          C.let_ (C.Tuple.snd center) @@ fun c_z ->
          C.Array.map vectors ~f:(fun v ->
              let open C.Float in
              let open C.Bool in
              C.Tuple_3.tuple_of v @@ fun (x, y, z) ->
              C.Tuple_3.tuple_of
                (inverse_rotate (x, y, z) (theta_x, theta_y, theta_z))
              @@ fun (rot_x, rot_y, rot_z) ->
              C.let_
                ( ((rot_y - c_y) ** float 2.0) + ((rot_z - c_z) ** float 2.0)
                < radius ** float 2.0 )
              @@ fun in_radius ->
              C.let_ (rot_x >= lo && rot_x <= hi) @@ fun in_height ->
              C.Bool.(in_radius && in_height))
      | App ("inter", [ e1; e2 ]) ->
          let v1 = eval ctx e1 |> to_examples
          and v2 = eval ctx e2 |> to_examples in
          examples @@ C.Array.map2 v1 v2 ~f:C.Bool.( && )
      | App ("union", [ e1; e2 ]) ->
          let v1 = eval ctx e1 |> to_examples
          and v2 = eval ctx e2 |> to_examples in
          examples @@ C.Array.map2 v1 v2 ~f:C.Bool.( || )
      | App ("sub", [ e1; e2 ]) ->
          let v1 = eval ctx e1 |> to_examples
          and v2 = eval ctx e2 |> to_examples in
          examples @@ C.Array.map2 v1 v2 ~f:C.Bool.(fun x1 x2 -> x1 && not x2)
      | App (var, []) -> (
          match Map.find ctx var with
          | Some x -> x
          | None ->
              failwith
              @@ Fmt.str "Unbound %s in %a" var
                   Fmt.Dump.(list @@ pair string Value.pp)
                   (Map.to_alist ctx) )
      | e ->
          Error.create "Unexpected expression." e
            [%sexp_of: Grammar.Untyped_term.t]
          |> Error.raise

    let eval ctx expr =
      try eval ctx (expr : [ `Closed ] Grammar.Term.t :> Grammar.Untyped_term.t)
      with exn ->
        let open Error in
        let err = of_exn exn in
        tag_arg err "Evaluation failed" expr [%sexp_of: _ Grammar.Term.t]
        |> raise

    let linear_int :
        _ ->
        _ Grammar.Term.t ->
        _ Grammar.Term.t ->
        (Value.t, bool code) Semantics.t Grammar.t =
     fun sym lo hi ->
      let open Grammar in
      let open Term in
      let lo_n = Bind.of_string "lo"
      and hi_n = Bind.of_string "hi"
      and v_n = Bind.of_string "v" in
      [
        Rule.create sym (app "range" [ lo; lo; hi ]) [];
        Rule.create sym
          (app "range"
             [ as_ lo lo_n; as_ (app "incr" [ nonterm sym ]) v_n; as_ hi hi_n ])
          [
            Semantics.Pred
              {
                deps = [ lo_n; hi_n; v_n ];
                func =
                  (fun ctx ->
                    eval ctx
                    @@ app "&&"
                         [
                           app ">=" [ app v_n []; app lo_n [] ];
                           app ">=" [ app hi_n []; app v_n [] ];
                         ]
                    |> Value.to_bool);
              };
          ];
      ]

    let grammar : (Value.t, bool code) Semantics.t Grammar.t =
      let open Grammar in
      let open Grammar.Term in
      let nt x = nonterm x in
      let cylinder_rule =
        let cylinder = Bind.of_string "cylinder"
        and lo_offset = Bind.of_string "lo_offset"
        and hi_offset = Bind.of_string "hi_offset" in
        let id_matches offset =
          Semantics.Pred
            {
              deps = [ offset; cylinder ];
              func =
                (fun ctx ->
                  let id =
                    Map.find_exn ctx cylinder |> Value.to_cylinder
                    |> C.Tuple_3.fst
                  and id' =
                    Map.find_exn ctx offset |> Value.to_offset |> C.Tuple.fst
                  in
                  C.Int.(id = id'));
            }
        in
        Rule.create "E"
          (app "cyl"
             [
               as_ (nt "C") cylinder;
               as_ (nt "CO") lo_offset;
               as_ (nt "CO") hi_offset;
               nt "V";
             ])
          [
            Semantics.Pred
              {
                deps = [ lo_offset; hi_offset ];
                func =
                  (fun ctx ->
                    let v =
                      Map.find_exn ctx lo_offset |> Value.to_offset
                      |> C.Tuple.snd
                    and v' =
                      Map.find_exn ctx hi_offset |> Value.to_offset
                      |> C.Tuple.snd
                    in
                    C.Float.(v < v'));
              };
            id_matches lo_offset;
            id_matches hi_offset;
          ]
      in
      Grammar.of_list
        [
          ("E", app "sphere" [ nt "S"; nt "V" ]);
          ("E", app "union" [ nt "E"; nt "E" ]);
          ("E", app "inter" [ nt "E"; nt "E" ]);
          ("E", app "sub" [ nt "E"; nt "E" ]);
        ]
      @ [ cylinder_rule ]
  end

  module Cache = struct
    type value = Lang.Value.t

    type 'a code = 'a C.t

    type cache = bool C.Array.t C.Set.t C.Array.t * int C.Set.t C.Array.t

    type t = cache C.t

    let max_size = 10

    open C

    let empty () =
      let examples_t = Array.mk_type @@ Set.mk_type @@ Array.mk_type Bool.type_
      and ints_t = Array.mk_type @@ Set.mk_type @@ Int.type_ in
      Nonlocal_let.let_ let_ (fun () ->
          Tuple.create
            (Array.init (Int.int max_size) (fun _ ->
                 Set.empty (Array.elem_type examples_t)))
            (Array.init (Int.int max_size) (fun _ ->
                 Set.empty (Array.elem_type ints_t))))

    let put ~sym ~size tbl v =
      match sym with
      | "E" -> Set.add (Tuple.fst tbl).(Int.int size) (Lang.Value.to_examples v)
      | "SI" -> Set.add (Tuple.snd tbl).(Int.int size) (Lang.Value.to_int v)
      | _ -> unit

    let iter ~sym ~size ~f tbl =
      match sym with
      | "E" ->
          Set.iter (Tuple.fst tbl).(size) (fun v -> f @@ Lang.Value.examples v)
      | "SI" -> Set.iter (Tuple.snd tbl).(size) (fun v -> f @@ Lang.Value.Int v)
      | _ -> unit

    let print_size _ = failwith "print_size"

    let code_of = Fun.id

    let of_code = Fun.id
  end
end
