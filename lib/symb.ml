open Ast
open Params

module Bool_vector = struct
  type elem = Free of Smt.Var.t | Fixed of bool [@@deriving sexp]

  type t = elem list [@@deriving sexp]

  let free x = Free x

  let fixed x = Fixed x

  let to_expr = function Free v -> Smt.var v | Fixed x -> Smt.bool x

  let create ?(prefix = sprintf "b%d") params =
    Smt.(
      List.init params.n_bits ~f:(fun b ->
          fresh_decl ~prefix:(prefix b) () >>| free)
      |> all)

  let of_abs ?(prefix = sprintf "b%d") params x =
    Smt.(
      List.init params ~f:(fun b ->
          match Map.find x b with
          | Some v -> return @@ fixed v
          | None -> fresh_decl ~prefix:(prefix b) () >>| free)
      |> all)

  let union x x' =
    Smt.(
      List.map2_exn x x' ~f:(fun v v' ->
          fresh_defn (to_expr v || to_expr v') >>| free)
      |> all)

  let inter x x' =
    Smt.(
      List.map2_exn x x' ~f:(fun v v' ->
          fresh_defn (to_expr v || to_expr v') >>| free)
      |> all)

  let sub x x' =
    Smt.(
      List.map2_exn x x' ~f:(fun v v' ->
          fresh_defn (to_expr v && not (to_expr v')) >>| free)
      |> all)

  let ( = ) x x' =
    Smt.(List.map2_exn x x' ~f:(fun v v' -> to_expr v = to_expr v') |> and_)

  let contained x by =
    let open Smt in
    List.filter_mapi x ~f:(fun b v ->
        match Map.find by b with
        | Some x -> Some (bool x = to_expr v)
        | None -> None)
    |> and_

  let refine models abs symb =
    let bit_idx =
      List.filter_mapi symb ~f:(fun i -> function
        | Free b -> Some (b, i) | _ -> None)
      |> Map.of_alist_exn (module Smt.Var)
    in

    let refined_states =
      Sequence.filter_map models ~f:(fun model ->
          Map.fold model ~init:(Some abs) ~f:(fun ~key:var ~data:value abs ->
              let open Option.Let_syntax in
              let%bind abs = abs in
              let%bind idx = Map.find bit_idx var in
              let%bind abs' = Abs.Bool_vector.add abs idx value in
              return abs'))
      |> Sequence.map ~f:Abs.bool_vector
      |> Sequence.to_list
      |> Set.of_list (module Abs)
    in
    refined_states
end

let offsets_of_type params t =
  params.bench.ops
  |> List.filter_map ~f:(function
       | Op.Offset x when [%compare.equal: Type.offset_type] x.Op.type_ t ->
           Some x.offset
       | _ -> None)

module Offset = struct
  type t = { set : Smt.Var.t list; type_ : Type.offset_type } [@@deriving sexp]

  let n_offsets params t = offsets_of_type params t |> List.length

  let create ?(prefix = sprintf "o%d") params t =
    let open Smt in
    let open Let_syntax in
    let%map set =
      n_offsets params t
      |> List.init ~f:(fun b -> fresh_decl ~prefix:(prefix b) ())
      |> all
    in
    { set; type_ = t }

  let ( = ) x x' =
    Smt.(List.map2_exn x.set x'.set ~f:(fun v v' -> var v = var v') |> and_)

  let contained params x by =
    List.map2_exn x.set (offsets_of_type params x.type_)
      ~f:(fun in_set offset ->
        let is_in = Abs.Offset.contains by offset in
        Smt.(var in_set = bool is_in))
    |> Smt.and_

  let index_of ~equal l x =
    List.find_mapi l ~f:(fun i x' -> if equal x x' then Some i else None)

  let split params (abs : Abs.Offset.t) symb var =
    index_of ~equal:[%compare.equal: Smt.Var.t] symb.set var
    |> Option.map ~f:(fun idx ->
           let split_offset =
             List.nth_exn (offsets_of_type params symb.type_) idx
           in
           ( { Abs.Offset.lo = abs.lo; hi = split_offset },
             { Abs.Offset.lo = split_offset; hi = abs.hi } ))

  let refine_single params (abs : Abs.Offset.t) symb var value =
    split params abs symb var
    |> Option.map ~f:(fun (t, f) -> if value then t else f)

  let refine params models old_abs symb =
    Sequence.map models ~f:(fun model ->
        Map.fold ~init:old_abs
          ~f:(fun ~key:var ~data:value abs ->
            refine_single params abs symb var value |> Option.value ~default:abs)
          model)
    |> Sequence.map ~f:Abs.offset |> Sequence.to_list
    |> Set.of_list (module Abs)

  let%expect_test "" =
    let s = String_id.of_string in
    let models =
      Sequence.singleton
      @@ Map.of_alist_exn (module String_id) [ (s "x0", false) ]
    in
    let type_ = Type.{ id = 0; kind = Cuboid_x } in
    let params =
      Params.create
      @@ Bench.
           {
             ops =
               [
                 Offset { offset = 10.0; type_ };
                 Offset { offset = 20.0; type_ };
                 Offset { offset = 30.0; type_ };
               ];
             input = [||];
             output = [||];
           }
    in
    refine params models
      { lo = Float.(-infinity); hi = Float.infinity }
      { set = [ s "x0"; s "x1"; s "x2" ]; type_ }
    |> [%sexp_of: Set.M(Abs).t] |> print_s
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

(* let of_abs ?prefix x =
 *   let open Smt.Monad_infix in
 *   function
 *   | Type.Vector -> Bool_vector.of_abs ?prefix x >>| bool_vector
 *   | Type.Offset t -> Offset.of_abs ?prefix t x >>| offset *)

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

let filter_offsets params (var : Offset.t) pred =
  List.map2_exn (offsets_of_type params var.type_) var.set
    ~f:(fun offset in_set ->
      if pred offset then Some (Smt.var in_set) else None)
  |> List.filter_map ~f:Fun.id

let cylinder params (c : Op.cylinder) l h =
  let l = to_offset_exn l and h = to_offset_exn h in
  let ret =
    params.bench.input |> Array.to_list
    |> List.map ~f:(fun v ->
           let open Vector3 in
           let rot = inverse_rotate v ~theta:c.theta in
           (* Check whether point is in radius. *)
           let in_radius =
             Float.(
               square (rot.y - c.y) + square (rot.z - c.z) < square c.radius)
           in
           (* Collect offsets that are above this point. *)
           let above = filter_offsets params l (fun o -> Float.(rot.x < o)) in
           (* Collect offsets that are below this point. *)
           let below = filter_offsets params h (fun o -> Float.(rot.x > o)) in
           (* Point is in if it is in the radius, none of the low offsets that are
              above it are selected, and none of the high offsets that are below
              it are selected *)
           Smt.(
             fresh_defn (bool in_radius && (not (or_ above)) && not (or_ below))
             >>| Bool_vector.free))
  in
  Smt.(all ret >>| bool_vector)

let cuboid params (c : Op.cuboid) lx hx ly hy lz hz =
  let lx = to_offset_exn lx
  and hx = to_offset_exn hx
  and ly = to_offset_exn ly
  and hy = to_offset_exn hy
  and lz = to_offset_exn lz
  and hz = to_offset_exn hz in
  let ret =
    params.bench.input |> Array.to_list
    |> List.map ~f:(fun v ->
           let open Vector3 in
           let rot = inverse_rotate v ~theta:c.theta in
           let above_x = filter_offsets params lx (fun o -> Float.(o > rot.x))
           and below_x = filter_offsets params hx (fun o -> Float.(o < rot.x))
           and above_y = filter_offsets params ly (fun o -> Float.(o > rot.y))
           and below_y = filter_offsets params hy (fun o -> Float.(o < rot.y))
           and above_z = filter_offsets params lz (fun o -> Float.(o > rot.z))
           and below_z =
             filter_offsets params hz (fun o -> Float.(o < rot.z))
           in
           Smt.(
             fresh_defn
               ( (not (or_ above_x))
               && (not (or_ below_x))
               && (not (or_ above_y))
               && (not (or_ below_y))
               && (not (or_ above_z))
               && not (or_ below_z) )
             >>| Bool_vector.free))
  in
  Smt.(all ret >>| bool_vector)

let sphere params (s : Op.sphere) =
  let ret =
    params.bench.input |> Array.to_list
    |> List.map ~f:(fun v ->
           Smt.(
             fresh_defn (bool Float.(Vector3.l2_dist s.center v <= s.radius))
             >>| Bool_vector.free))
  in
  Smt.(all ret >>| bool_vector)

let offset params (o : Op.offset) =
  let open Smt.Let_syntax in
  let%map set =
    List.map (offsets_of_type params o.type_) ~f:(fun offset ->
        Smt.fresh_defn (Smt.bool Float.(o.offset = offset)))
    |> Smt.all
  in
  Offset.{ type_ = o.type_; set } |> offset

let eval params op args =
  let open Util in
  match op with
  | Op.Union -> apply2 union args
  | Inter -> apply2 inter args
  | Sub -> apply2 sub args
  | Cylinder c -> apply2 (cylinder params c) args
  | Cuboid c -> apply6 (cuboid params c) args
  | Sphere s -> sphere params s
  | Offset o -> offset params o

let refine params models abs symb =
  print_s
    [%message
      "refine"
        (models : Smt.Model.t Sequence.t)
        (symb : t)
        (abs : Abs.t)
        [%here]];

  map
    ~vector:(fun s -> Bool_vector.refine models (Abs.to_bool_vector_exn abs) s)
    ~offset:(fun s -> Offset.refine params models (Abs.to_offset_exn abs) s)
    symb
