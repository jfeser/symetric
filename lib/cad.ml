open Types

module type Deps = sig
  include Sigs.CODE

  module Set :
    Cstage_set.S with type 'a code := 'a t and type 'a ctype := 'a ctype

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

      type offset_t = C.Int.t * C.Float.t

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

      type cuboid_t =
        ( int (* id *),
          C.Float.t (* theta_x *),
          C.Float.t (* theta_y *),
          C.Float.t (* theta_z *) )
        C.Tuple_4.t

      type t =
        | Examples of bool C.Array.t C.t
        | Vectors of vectors_t C.t
        | Sphere of sphere_t C.t
        | Cylinder of cylinder_t C.t
        | Cylinder_offset of offset_t C.t
        | Cuboid of cuboid_t C.t
        | Cuboid_x_offset of offset_t C.t
        | Cuboid_y_offset of offset_t C.t
        | Cuboid_z_offset of offset_t C.t
        | Bool of bool C.t
        | Int of int C.t
      [@@deriving variants]

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

      let to_cuboid = function Cuboid x -> x | x -> err "cuboid" x

      let to_bool = function Bool x -> x | x -> err "bool" x

      let to_int = function Int x -> x | x -> err "int" x

      let to_offset = function
        | Cylinder_offset x
        | Cuboid_x_offset x
        | Cuboid_y_offset x
        | Cuboid_z_offset x ->
            x
        | x -> err "offset" x

      let to_id = function
        | Cylinder_offset x
        | Cuboid_x_offset x
        | Cuboid_y_offset x
        | Cuboid_z_offset x ->
            C.Tuple.fst x
        | Cylinder x -> C.Tuple_3.fst x
        | Cuboid x -> C.Tuple_4.fst x
        | x -> err "has id" x

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
        let cuboid_of_sexp s =
          C.Tuple_4.of_sexp s C.Int.of_sexp C.Float.of_sexp C.Float.of_sexp
            C.Float.of_sexp
        in
        let vectors_of_sexp s =
          C.Array.of_sexp s @@ fun s ->
          C.Tuple_3.of_sexp s C.Float.of_sexp C.Float.of_sexp C.Float.of_sexp
        in
        let offset_of_sexp s =
          C.Tuple.of_sexp s C.Int.of_sexp C.Float.of_sexp
        in
        if Char.(sym.[0] = 'E') then Examples (examples_of_sexp sexp)
        else if String.(sym = "S") then Sphere (sphere_of_sexp sexp)
        else if String.(sym = "V") then Vectors (vectors_of_sexp sexp)
        else if String.(sym = "C") then Cylinder (cylinder_of_sexp sexp)
        else if String.(sym = "CO") then Cylinder_offset (offset_of_sexp sexp)
        else if String.(sym = "U") then Cuboid (cuboid_of_sexp sexp)
        else if String.(sym = "UOX") then Cuboid_x_offset (offset_of_sexp sexp)
        else if String.(sym = "UOY") then Cuboid_y_offset (offset_of_sexp sexp)
        else if String.(sym = "UOZ") then Cuboid_z_offset (offset_of_sexp sexp)
        else failwith "Unexpected symbol"

      let ( = ) v v' =
        match (v, v') with
        | Examples a, Examples a' -> `Dyn C.Array.O.(a = a')
        | Int x, Int x' -> `Dyn C.Int.(x = x')
        | _ -> failwith "Cannot compare"

      module Key = struct
        type value = t

        type t = K : (_ C.t -> value) Variant.t -> t

        let sexp_of_t (K _) = assert false

        let key = Univ_map.Key.create ~name:"cad.value" [%sexp_of: t]
      end

      let code_of v =
        let conv variant value =
          C.add_annot (C.cast value) Key.key (Key.K variant)
        in
        Variants.map ~examples:conv ~vectors:conv ~sphere:conv ~cylinder:conv
          ~cylinder_offset:conv ~cuboid:conv ~cuboid_x_offset:conv
          ~cuboid_y_offset:conv ~cuboid_z_offset:conv ~bool:conv ~int:conv v

      let of_code c =
        match C.find_annot c Key.key with
        | Some (Key.K v) -> v.Variant.constructor @@ C.cast c
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

    module C2 = C.Tuple
    module C3 = C.Tuple_3
    module C4 = C.Tuple_4

    let eval_cuboid cuboid_hint x_lo x_hi y_lo y_hi z_lo z_hi vectors =
      C4.tuple_of cuboid_hint @@ fun (id, theta_x, theta_y, theta_z) ->
      C.Array.map vectors ~f:(fun v ->
          C3.tuple_of v @@ fun (x, y, z) ->
          C3.tuple_of (inverse_rotate (x, y, z) (theta_x, theta_y, theta_z))
          @@ fun (rot_x, rot_y, rot_z) ->
          let open C.Float in
          let open C.Bool in
          rot_x >= x_lo && rot_x <= x_hi && rot_y >= y_lo && rot_y <= y_hi
          && rot_z >= z_lo && rot_z <= z_hi)

    let rec eval ctx = function
      | Grammar.As (t, _) -> eval ctx t
      | App ("sphere", [ s; v ]) ->
          let sphere = to_sphere (eval ctx s)
          and vectors = to_vectors (eval ctx v) in
          examples @@ C4.tuple_of sphere
          @@ fun (x, y, z, r) ->
          C.Array.map vectors ~f:(fun v ->
              C3.tuple_of v @@ fun (x', y', z') ->
              C.Float.(
                ((x - x') ** float 2.0)
                + ((y - y') ** float 2.0)
                + ((z - z') ** float 2.0)
                < r ** float 2.0))
      | App ("cyl", [ c; lo; hi; v ]) ->
          let cyl = to_cylinder @@ eval ctx c
          and lo = eval_offset ctx lo
          and hi = eval_offset ctx hi
          and vectors = to_vectors @@ eval ctx v in
          examples @@ C3.tuple_of cyl
          @@ fun (_, disc, center) ->
          C4.tuple_of disc @@ fun (theta_x, theta_y, theta_z, radius) ->
          C.let_ (C2.fst center) @@ fun c_y ->
          C.let_ (C2.snd center) @@ fun c_z ->
          C.Array.map vectors ~f:(fun v ->
              let open C.Float in
              let open C.Bool in
              C3.tuple_of v @@ fun (x, y, z) ->
              C3.tuple_of (inverse_rotate (x, y, z) (theta_x, theta_y, theta_z))
              @@ fun (rot_x, rot_y, rot_z) ->
              C.let_
                ( ((rot_y - c_y) ** float 2.0) + ((rot_z - c_z) ** float 2.0)
                < radius ** float 2.0 )
              @@ fun in_radius ->
              C.let_ (rot_x >= lo && rot_x <= hi) @@ fun in_height ->
              in_radius && in_height)
      | App ("cuboid", [ c; xl; xh; yl; yh; zl; zh; v ]) ->
          examples
          @@ eval_cuboid
               (eval ctx c |> to_cuboid)
               (eval_offset ctx xl) (eval_offset ctx xh) (eval_offset ctx yl)
               (eval_offset ctx yh) (eval_offset ctx zl) (eval_offset ctx zh)
               (eval ctx v |> to_vectors)
      | App ("inter", [ e1; e2 ]) ->
          let v1 = eval_examples ctx e1 and v2 = eval_examples ctx e2 in
          examples @@ C.Array.map2 v1 v2 ~f:C.Bool.( && )
      | App ("union", [ e1; e2 ]) ->
          let v1 = eval_examples ctx e1 and v2 = eval_examples ctx e2 in
          examples @@ C.Array.map2 v1 v2 ~f:C.Bool.( || )
      | App ("sub", [ e1; e2 ]) ->
          let v1 = eval_examples ctx e1 and v2 = eval_examples ctx e2 in
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

    and eval_examples ctx x = eval ctx x |> to_examples

    and eval_offset ctx x = eval ctx x |> to_offset |> C2.snd

    let eval ctx expr =
      try eval ctx (expr : [ `Closed ] Grammar.Term.t :> Grammar.Untyped_term.t)
      with exn ->
        let open Error in
        let err = of_exn exn in
        tag_arg err "Evaluation failed" expr [%sexp_of: _ Grammar.Term.t]
        |> raise

    let grammar : (Value.t, bool code) Semantics.t Grammar.t =
      let open Grammar in
      let open Grammar.Term in
      let nt x = nonterm x in

      let id_matches x y =
        let func ctx =
          let id = Map.find_exn ctx x |> Value.to_id
          and id' = Map.find_exn ctx y |> Value.to_id in
          C.Int.(id = id')
        in
        Semantics.Pred { deps = [ x; y ]; func }
      in

      let offset_lt lo hi =
        let func ctx =
          let v = Map.find_exn ctx lo |> Value.to_offset |> C2.snd
          and v' = Map.find_exn ctx hi |> Value.to_offset |> C2.snd in
          C.Float.(v < v')
        in
        Semantics.Pred { deps = [ lo; hi ]; func }
      in

      let cylinder_rule =
        let cylinder = Bind.of_string "cylinder"
        and lo = Bind.of_string "lo"
        and hi = Bind.of_string "hi" in
        Rule.create "E"
          (app "cyl"
             [
               as_ (nt "C") cylinder; as_ (nt "CO") lo; as_ (nt "CO") hi; nt "V";
             ])
          [ id_matches cylinder lo; id_matches cylinder hi; offset_lt lo hi ]
      and cuboid_rule =
        let cuboid = Bind.of_string "cuboid"
        and x_lo = Bind.of_string "x_lo"
        and y_lo = Bind.of_string "y_lo"
        and z_lo = Bind.of_string "z_lo"
        and x_hi = Bind.of_string "x_hi"
        and y_hi = Bind.of_string "y_hi"
        and z_hi = Bind.of_string "z_hi" in
        let preds =
          List.map [ x_lo; x_hi; y_lo; y_hi; z_lo; z_hi ] ~f:(id_matches cuboid)
          @ [ offset_lt x_lo x_hi; offset_lt y_lo y_hi; offset_lt z_lo z_hi ]
        in

        Rule.create "E"
          (app "cuboid"
             [
               as_ (nt "U") cuboid;
               as_ (nt "UOX") x_lo;
               as_ (nt "UOX") x_hi;
               as_ (nt "UOY") y_lo;
               as_ (nt "UOY") y_hi;
               as_ (nt "UOZ") z_lo;
               as_ (nt "UOZ") z_hi;
               nt "V";
             ])
          preds
      in

      Grammar.of_list
        [
          ("E0", app "union" [ app "union" [ nt "E"; nt "E" ]; nt "E" ]);
          ("E", app "sphere" [ nt "S"; nt "V" ]);
          ("E", app "union" [ nt "E"; nt "E" ]);
          ("E", app "inter" [ nt "E"; nt "E" ]);
          ("E", app "sub" [ nt "E"; nt "E" ]);
        ]
      @ [ cylinder_rule; cuboid_rule ]

    let cost = function
      | "sphere" | "cuboid" | "cyl" | "union" | "inter" | "sub" -> 1
      | sym -> failwith @@ "Unknown symbol: " ^ sym
  end

  module Cache = struct
    type value = Lang.Value.t

    type 'a code = 'a C.t

    type cache = bool C.Array.t C.Set.t C.Array.t * int C.Set.t C.Array.t

    type t = cache C.t

    let max_size = 50

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
