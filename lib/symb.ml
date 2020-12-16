open Ast
open Params

module Bool_vector = struct
  type elem = Free of Smt.Var.t | Fixed of bool [@@deriving compare, sexp]

  type t = elem list [@@deriving sexp]

  let free x = Free x

  let fixed x = Fixed x

  let to_expr = function Free v -> Smt.var v | Fixed x -> Smt.bool x

  let create_n ?(prefix = sprintf "b%d") n =
    Smt.(
      List.init n ~f:(fun b -> fresh_decl ~prefix:(prefix b) () >>| free) |> all)

  let create ?prefix params = create_n ?prefix params.n_bits

  let of_abs ?(prefix = sprintf "b%d") ~n_bits = function
    | Abs.Bool_vector.Map m ->
        Smt.(
          List.init n_bits ~f:(fun b ->
              match Map.find m b with
              | Some v -> return @@ fixed v
              | None -> fresh_decl ~prefix:(prefix b) () >>| free)
          |> all)
    | Bottom -> failwith "bottom"

  let exactly_one x =
    List.map x ~f:(function Fixed x -> Smt.bool x | Free v -> Smt.var v)
    |> Smt.exactly_one

  let union x x' =
    Smt.(
      List.map2_exn x x' ~f:(fun v v' ->
          fresh_defn (to_expr v || to_expr v') >>| free)
      |> all)

  let inter x x' =
    Smt.(
      List.map2_exn x x' ~f:(fun v v' ->
          fresh_defn (to_expr v && to_expr v') >>| free)
      |> all)

  let sub x x' =
    Smt.(
      List.map2_exn x x' ~f:(fun v v' ->
          fresh_defn (to_expr v && not (to_expr v')) >>| free)
      |> all)

  let ( = ) x x' =
    Smt.(List.map2_exn x x' ~f:(fun v v' -> to_expr v = to_expr v') |> and_)

  let get = List.nth_exn

  let contained x by =
    let open Smt in
    match by with
    | Abs.Bool_vector.Map m ->
        List.filter_mapi x ~f:(fun b v ->
            match Map.find m b with
            | Some x -> Some (bool x = to_expr v)
            | None -> None)
        |> and_
    | Bottom -> bool false

  let refine models abs symb =
    let bit_idx =
      List.filter_mapi symb ~f:(fun i -> function
        | Free b -> Some (b, i) | _ -> None)
      |> Map.of_alist_exn (module Smt.Var)
    in

    let refined_states =
      Sequence.filter_map models ~f:(fun model ->
          Map.fold model ~init:(Some abs) ~f:(fun ~key:var ~data:value abs ->
              let idx = Map.find_exn bit_idx var in
              Option.bind abs ~f:(fun abs ->
                  let open Abs.Bool_vector in
                  let abs' = add abs idx value in
                  if [%compare.equal: t] abs' bot then None else Some abs')))
      |> Sequence.map ~f:Abs.bool_vector
      |> Sequence.to_list
      |> Set.of_list (module Abs)
    in
    refined_states

  let vars x =
    List.filter_map x ~f:(function Free v -> Some v | Fixed _ -> None)
    |> Set.of_list (module Smt.Var)
end

module Offset' = Offset

module Offset = struct
  type t = { set : Bool_vector.t; type_ : Offset_type.t } [@@deriving sexp]

  let vars x = Bool_vector.vars x.set

  let n_offsets params = Offset.of_type_count params.offsets

  let create ?(prefix = sprintf "o%d") params t =
    let open Smt in
    let open Let_syntax in
    let%map set = Bool_vector.create_n ~prefix @@ n_offsets params t in
    { set; type_ = t }

  let of_abs ?(prefix = sprintf "o%d") offsets group abs =
    let open Smt.Let_syntax in
    let type_ = Abs.Offset.type_ abs in
    let n = Offset.of_type_count offsets type_ in
    let%bind set =
      let map =
        Offset.of_type offsets type_
        |> Sequence.filter_mapi ~f:(fun i conc ->
               if not (Abs.Offset.contains abs conc) then Some (i, false)
               else None)
        |> Map.of_sequence_exn (module Int)
      in
      Bool_vector.of_abs ~prefix ~n_bits:n (Map map)
    in
    let%map () =
      Smt.Interpolant.assert_group ~group @@ Bool_vector.exactly_one set
    in
    { set; type_ }

  let ( = ) x x' = Bool_vector.(x.set = x'.set)

  let contained params x by =
    Sequence.zip (Sequence.of_list x.set)
      (Offset.of_type params.offsets x.type_)
    |> Sequence.filter_map ~f:(fun (in_set, offset) ->
           if not (Abs.Offset.contains by offset) then
             Some Smt.(not (Bool_vector.to_expr in_set))
           else None)
    |> Sequence.to_list |> Smt.and_

  let index_of_exn ~equal l x =
    List.find_mapi_exn l ~f:(fun i x' -> if equal x x' then Some i else None)

  let refine_with_models params models old_abs symb =
    let should_keep =
      Sequence.reduce_exn models
        ~f:
          (Map.merge ~f:(fun ~key:_ -> function
             | `Both (false, false) -> Some false
             | `Both (false, true) | `Both (true, _) -> Some true
             | `Left _ | `Right _ -> None))
      |> Map.filter_keys ~f:(fun v ->
             List.mem symb.set ~equal:[%compare.equal: Bool_vector.elem]
               (Free v))
    in

    let offset_of_var =
      let offsets = Offset.of_type params.offsets symb.type_ in
      let offset_of_var =
        Sequence.zip (Sequence.of_list symb.set) offsets
        |> Sequence.filter_map ~f:(fun (var, offset) ->
               match var with
               | Bool_vector.Free v -> Some (v, offset)
               | Fixed _ -> None)
        |> Map.of_sequence_exn (module Smt.Var)
      in
      Map.find_exn offset_of_var
    in

    let refined =
      Map.to_alist should_keep
      |> List.fold ~init:[ old_abs ] ~f:(fun refined (var, keep) ->
             let offset = offset_of_var var in
             if keep then
               List.concat_map refined ~f:(fun abs ->
                   if Abs.Offset.contains abs offset then
                     Abs.Offset.split_exn abs offset
                   else [ abs ])
             else
               List.concat_map refined ~f:(fun abs ->
                   if Abs.Offset.contains abs offset then
                     Abs.Offset.exclude_exn abs offset
                   else [ abs ]))
    in

    refined |> List.map ~f:Abs.offset |> Set.of_list (module Abs)

  let refine = refine_with_models

  let v x = String_id.of_string x

  let f x = Bool_vector.Free (v x)

  let simple_test models =
    let models =
      Sequence.of_list models
      |> Sequence.map ~f:(Map.of_alist_exn (module String_id))
    in
    let type_ = Offset_type.{ id = 0; kind = Cuboid_x } in
    let params =
      Params.create
      @@ Bench.
           {
             ops =
               [
                 Offset { offset = 2.0; type_ };
                 Offset { offset = 5.0; type_ };
                 Offset { offset = 10.0; type_ };
                 Offset { offset = 20.0; type_ };
                 Offset { offset = 30.0; type_ };
               ];
             input = [||];
             output = [||];
           }
    in
    refine params models (Abs.Offset.top type_)
      { set = [ f "y0"; f "y1"; f "x0"; f "x1"; f "x2" ]; type_ }
    |> [%sexp_of: Set.M(Abs).t] |> print_s

  let%expect_test "" =
    simple_test [ [ (v "x0", false) ] ];
    [%expect
      {|
      ((Offset ((lo -INF) (hi 5) (type_ ((id 0) (kind Cuboid_x)))))
       (Offset ((lo 20) (hi INF) (type_ ((id 0) (kind Cuboid_x)))))) |}]

  let%expect_test "" =
    simple_test
      [
        [ (v "x0", false); (v "x1", true); (v "x2", true) ];
        [ (v "x0", false); (v "x1", false); (v "x2", true) ];
      ];
    [%expect
      {|
      ((Offset ((lo -INF) (hi 5) (type_ ((id 0) (kind Cuboid_x)))))
       (Offset ((lo 20) (hi 20) (type_ ((id 0) (kind Cuboid_x)))))
       (Offset ((lo 30) (hi 30) (type_ ((id 0) (kind Cuboid_x)))))) |}]
end

type t = Bool_vector of Bool_vector.t | Offset of Offset.t [@@deriving sexp]

let bool_vector x = Bool_vector x

let offset x = Offset x

let to_bool_vector_exn = function
  | Bool_vector x -> x
  | v -> raise_s [%message "Expected a bool vector" (v : t)]

let to_offset_exn = function
  | Offset x -> x
  | v -> raise_s [%message "Expected an offset" (v : t)]

let create ?prefix params =
  let open Smt.Monad_infix in
  function
  | Type.Vector -> Bool_vector.create ?prefix params >>| bool_vector
  | Type.Offset t -> Offset.create ?prefix params t >>| offset

let of_abs ?prefix params group =
  let open Smt.Monad_infix in
  function
  | Abs.Bool_vector v ->
      Bool_vector.of_abs ?prefix ~n_bits:params.Params.n_bits v >>| bool_vector
  | Abs.Offset v -> Offset.of_abs ?prefix params.offsets group v >>| offset

let map ~vector ~offset x =
  match x with Bool_vector x -> vector x | Offset x -> offset x

let map2 ~vector ~offset x x' =
  match (x, x') with
  | Bool_vector x, Bool_vector x' -> vector x x'
  | Offset x, Offset x' -> offset x x'
  | _ -> raise_s [%message "Mismatched values" (x : t) (x' : t)]

let map_abs ~vector ~offset x x' =
  match (x, x') with
  | Bool_vector x, Abs.Bool_vector x' -> vector x x'
  | Offset x, Abs.Offset x' -> offset x x'
  | _ -> raise_s [%message "Mismatched values" (x : t) (x' : Abs.t)]

let ( = ) = map2 ~vector:Bool_vector.( = ) ~offset:Offset.( = )

let contained params x ~by =
  map_abs ~vector:Bool_vector.(contained) ~offset:Offset.(contained params) x by

let bool_vector_3 f x x' =
  Smt.(f (to_bool_vector_exn x) (to_bool_vector_exn x') >>| bool_vector)

let union = bool_vector_3 Bool_vector.union

let inter = bool_vector_3 Bool_vector.inter

let sub = bool_vector_3 Bool_vector.sub

let filter_offsets offsets (var : Offset.t) pred =
  Sequence.zip (Offset'.of_type offsets var.type_) (Sequence.of_list var.set)
  |> Sequence.filter_map ~f:(fun (offset, in_set) ->
         if pred offset then Some (Bool_vector.to_expr in_set) else None)
  |> Sequence.to_list

let cylinder (c : Op.cylinder) input offsets l h =
  let open Smt.Monad_infix in
  List.map (Array.to_list input) ~f:(fun v ->
      let open Vector3 in
      let rot = inverse_rotate v ~theta:c.theta in
      (* Check whether point is in radius. *)
      let in_radius =
        Float.(square (rot.y - c.y) + square (rot.z - c.z) < square c.radius)
      in
      (* Collect offsets that are above this point. *)
      let above =
        filter_offsets offsets l (fun o -> Float.(rot.x < Offset'.offset o))
      in
      (* Collect offsets that are below this point. *)
      let below =
        filter_offsets offsets h (fun o -> Float.(rot.x > Offset'.offset o))
      in
      (* Point is in if it is in the radius, none of the low offsets that are
         above it are selected, and none of the high offsets that are below it
         are selected *)
      Smt.(fresh_defn (bool in_radius && (not (or_ above)) && not (or_ below)))
      >>| Bool_vector.free)
  |> Smt.all

let cuboid (c : Op.cuboid) input offsets lx hx ly hy lz hz =
  let open Smt.Monad_infix in
  Array.to_list input
  |> List.map ~f:(fun v ->
         let open Vector3 in
         let rot = inverse_rotate v ~theta:c.theta in
         let above_x =
           filter_offsets offsets lx (fun o -> Float.(Offset'.offset o > rot.x))
         and below_x =
           filter_offsets offsets hx (fun o -> Float.(Offset'.offset o < rot.x))
         and above_y =
           filter_offsets offsets ly (fun o -> Float.(Offset'.offset o > rot.y))
         and below_y =
           filter_offsets offsets hy (fun o -> Float.(Offset'.offset o < rot.y))
         and above_z =
           filter_offsets offsets lz (fun o -> Float.(Offset'.offset o > rot.z))
         and below_z =
           filter_offsets offsets hz (fun o -> Float.(Offset'.offset o < rot.z))
         in
         Smt.(
           fresh_defn
             ( (not (or_ above_x))
             && (not (or_ below_x))
             && (not (or_ above_y))
             && (not (or_ below_y))
             && (not (or_ above_z))
             && not (or_ below_z) ))
         >>| Bool_vector.free)
  |> Smt.all >>| bool_vector

let sphere params (s : Op.sphere) =
  let ret =
    params.bench.input |> Array.to_list
    |> List.map ~f:(fun v ->
           Smt.(
             fresh_defn (bool Float.(Vector3.l2_dist s.center v <= s.radius))
             >>| Bool_vector.free))
  in
  Smt.(all ret >>| bool_vector)

let offset params o =
  let open Smt.Let_syntax in
  let type_ = Offset'.type_ o in
  let%map set =
    Offset'.of_type params.offsets type_
    |> Sequence.map ~f:(fun offset ->
           return @@ Bool_vector.fixed @@ [%compare.equal: Offset'.t] o offset)
    |> Sequence.to_list |> Smt.all
  in
  Offset.{ type_; set } |> offset

let eval params op args =
  let open Smt.Monad_infix in
  let open Util in
  let eval_offsets = List.map ~f:to_offset_exn in
  match op with
  | Op.Union -> apply2 union args
  | Inter -> apply2 inter args
  | Sub -> apply2 sub args
  | Cylinder c ->
      apply2 (cylinder c params.bench.input params.offsets) @@ eval_offsets args
      >>| bool_vector
  | Cuboid c ->
      apply6 (cuboid c params.bench.input params.offsets) @@ eval_offsets args
  | Sphere s -> sphere params s
  | Offset o -> offset params o

let assert_refines =
  [%test_pred: Abs.t * Set.M(Abs).t] (fun (old_abs, new_abs) ->
      Set.for_all new_abs ~f:(fun a -> Abs.is_subset a ~of_:old_abs))

let refine params interpolant lower_constr abs symb =
  let models =
    let vars = map ~vector:Bool_vector.vars ~offset:Offset.vars symb in
    Smt.Model.of_ ~vars interpolant
  in
  let filtered_models =
    Sequence.filter models ~f:(fun m ->
        let check_model =
          let open Smt.Let_syntax in
          let%bind () = lower_constr in
          let%bind () = Smt.assert_ (Smt.Model.to_expr m) in
          Smt.check_sat
        in
        Smt.eval check_model)
  in
  let refined =
    map
      ~vector:(fun s ->
        Bool_vector.refine filtered_models (Abs.to_bool_vector_exn abs) s)
      ~offset:(fun s ->
        Offset.refine params filtered_models (Abs.to_offset_exn abs) s)
      symb
  in
  assert_refines (abs, refined);
  print_s
    [%message
      "refine"
        (models : Smt.Model.t Sequence.t)
        (filtered_models : Smt.Model.t Sequence.t)
        (symb : t)
        (abs : Abs.t)
        (refined : Set.M(Abs).t)
        [%here]];
  refined

let vars = function
  | Bool_vector v -> Bool_vector.vars v
  | Offset v -> Offset.vars v
