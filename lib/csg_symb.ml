open Params

module Bool_vector = struct
  include Symb0.Bool_vector

  let elem_of_expr =
    Smt.fresh_defn_or_literal
      ~defn:(fun v -> free v |> Smt.return)
      ~literal:(fun v -> fixed v |> Smt.return)

  let create_n ?(prefix = sprintf "b%d") n =
    Smt.(
      List.init n ~f:(fun b -> fresh_decl ~prefix:(prefix b) () >>| free) |> all)

  let create ?prefix params = create_n ?prefix (Csg_bench.n_bits params.bench)

  let of_conc x = Array.to_list x |> List.map ~f:fixed

  let union x x' =
    Smt.(
      List.map2_exn x x' ~f:(fun v v' -> elem_of_expr (to_expr v || to_expr v'))
      |> all)

  let inter x x' =
    Smt.(
      List.map2_exn x x' ~f:(fun v v' -> elem_of_expr (to_expr v && to_expr v'))
      |> all)

  let sub x x' =
    Smt.(
      List.map2_exn x x' ~f:(fun v v' ->
          elem_of_expr (to_expr v && not (to_expr v')))
      |> all)

  let ( = ) x x' =
    Smt.(List.map2_exn x x' ~f:(fun v v' -> to_expr v = to_expr v') |> and_)

  let get = List.nth_exn

  let refine models abs symb =
    let bit_idx =
      List.filter_mapi symb ~f:(fun i -> function
        | Free b -> Some (b, i) | _ -> None)
      |> Map.of_alist_exn (module Smt.Var)
    in

    let refined_states =
      List.filter_map models ~f:(fun model ->
          Map.fold model ~init:(Some abs) ~f:(fun ~key:var ~data:value abs ->
              let idx = Map.find_exn bit_idx var in
              Option.bind abs ~f:(fun abs ->
                  let open Csg_abs.Bool_vector in
                  let abs' = add abs idx value in
                  if [%compare.equal: t] abs' bot then None else Some abs')))
      |> List.map ~f:Csg_abs.bool_vector
      |> Set.of_list (module Csg_abs)
    in
    refined_states

  let vars x =
    List.filter_map x ~f:(function Free v -> Some v | Fixed _ -> None)
    |> Set.of_list (module Smt.Var)
end

module Offset' = Offset

module Offset = struct
  include Symb0.Offset

  let vars x = Bool_vector.vars x.set

  let n_offsets params = Offset.of_type_count (Csg_bench.offsets params.bench)

  let create ?(prefix = sprintf "o%d") params t =
    let open Smt in
    let open Let_syntax in
    let%map set = Bool_vector.create_n ~prefix @@ n_offsets params t in
    { set; type_ = t }

  let of_conc ctx x =
    let type_ = Offset.type_ x in
    let set =
      Offset.of_type ctx type_
      |> Sequence.map ~f:(fun x' ->
             Bool_vector.fixed ([%compare.equal: Offset.t] x x'))
      |> Sequence.to_list
    in
    { set; type_ }

  let ( = ) x x' = Bool_vector.(x.set = x'.set)

  let index_of_exn ~equal l x =
    List.find_mapi_exn l ~f:(fun i x' -> if equal x x' then Some i else None)

  let refine_with_models params models old_abs symb =
    let should_keep =
      List.reduce_exn models
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
      let offsets =
        Offset.of_type (Csg_bench.offsets params.bench) symb.type_
      in
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
                   if Csg_abs.Offset.contains abs offset then
                     Csg_abs.Offset.split_exn abs offset
                   else [ abs ])
             else
               List.concat_map refined ~f:(fun abs ->
                   if Csg_abs.Offset.contains abs offset then
                     Csg_abs.Offset.exclude_exn abs offset
                   else [ abs ]))
    in

    refined |> List.map ~f:Csg_abs.offset |> Set.of_list (module Csg_abs)

  let refine = refine_with_models

  let v x = String_id.of_string x

  let f x = Bool_vector.Free (v x)
end

type t = Symb0.t = Bool_vector of Bool_vector.t | Offset of Offset.t
[@@deriving sexp]

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
  | Csg_type.Vector -> Bool_vector.create ?prefix params >>| bool_vector
  | Offset t -> Offset.create ?prefix params t >>| offset

let map ~vector ~offset x =
  match x with Bool_vector x -> vector x | Offset x -> offset x

let map2 ~vector ~offset x x' =
  match (x, x') with
  | Bool_vector x, Bool_vector x' -> vector x x'
  | Offset x, Offset x' -> offset x x'
  | _ -> raise_s [%message "Mismatched values" (x : t) (x' : t)]

let map_abs ~vector ~offset x x' =
  match (x, x') with
  | Bool_vector x, Csg_abs.Bool_vector x' -> vector x x'
  | Offset x, Csg_abs.Offset x' -> offset x x'
  | _ -> raise_s [%message "Mismatched values" (x : t) (x' : Csg_abs.t)]

let equals = map2 ~vector:Bool_vector.( = ) ~offset:Offset.( = )

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

let cylinder (c : Csg_op.cylinder) input offsets l h =
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
      Bool_vector.elem_of_expr
        Smt.(bool in_radius && (not (or_ above)) && not (or_ below)))
  |> Smt.all

let cuboid (c : Csg_op.cuboid) input offsets lx hx ly hy lz hz =
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
         Bool_vector.elem_of_expr
           Smt.(
             (not (or_ above_x))
             && (not (or_ below_x))
             && (not (or_ above_y))
             && (not (or_ below_y))
             && (not (or_ above_z))
             && not (or_ below_z)))
  |> Smt.all >>| bool_vector

let sphere params (s : Csg_op.sphere) =
  let ret =
    params.bench.Csg_bench.input |> Array.to_list
    |> List.map ~f:(fun v ->
           Bool_vector.elem_of_expr
           @@ Smt.bool Float.(Vector3.l2_dist s.center v <= s.radius))
  in
  Smt.(all ret >>| bool_vector)

let offset params o =
  let open Smt.Let_syntax in
  let type_ = Offset'.type_ o in
  let%map set =
    Offset'.of_type params.bench.Csg_bench.offsets type_
    |> Sequence.map ~f:(fun offset ->
           return @@ Bool_vector.fixed @@ [%compare.equal: Offset'.t] o offset)
    |> Sequence.to_list |> Smt.all
  in
  Offset.{ type_; set } |> offset

let eval params op args =
  let open Smt.Monad_infix in
  let open Apply in
  let eval_offsets = List.map ~f:to_offset_exn in
  match op with
  | Csg_op.Union -> apply2 union args
  | Inter -> apply2 inter args
  | Sub -> apply2 sub args
  | Cylinder c ->
      apply2 (cylinder c params.bench.Csg_bench.input params.bench.offsets)
      @@ eval_offsets args
      >>| bool_vector
  | Cuboid c ->
      apply6 (cuboid c params.bench.input params.bench.offsets)
      @@ eval_offsets args
  | Sphere s -> sphere params s
  | Offset o -> offset params o

let assert_refines =
  [%test_pred: Csg_abs.t * Set.M(Csg_abs).t] (fun (old_abs, new_abs) ->
      Set.for_all new_abs ~f:(fun a -> Csg_abs.is_subset a ~of_:old_abs))

let refine params interpolant smt_state abs symb =
  let vars =
    Set.to_list
    @@ Set.inter
         (map ~vector:Bool_vector.vars ~offset:Offset.vars symb)
         (Smt.Expr.vars interpolant)
  in
  let models =
    let smt =
      let open Smt.Let_syntax in
      let%bind () = Smt.assert_ interpolant in
      Smt.check_all_sat vars
    in
    Smt.eval_with_state smt_state smt
  in
  let refined =
    map
      ~vector:(fun s ->
        Bool_vector.refine models (Csg_abs.to_bool_vector_exn abs) s)
      ~offset:(fun s ->
        Offset.refine params models (Csg_abs.to_offset_exn abs) s)
      symb
  in
  assert_refines (abs, refined);
  print_s
    [%message
      "refine"
        (models : Smt.Model.t list)
        (symb : t)
        (abs : Csg_abs.t)
        (refined : Set.M(Csg_abs).t)
        [%here]];
  refined

let vars = function
  | Bool_vector v -> Bool_vector.vars v
  | Offset v -> Offset.vars v

let of_conc params = function
  | Csg_conc.Bool_vector x -> Bool_vector (Bool_vector.of_conc x)
  | Offset x -> Offset (Offset.of_conc params.bench.Csg_bench.offsets x)
