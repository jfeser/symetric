open Ast

module Bool_vector = struct
  type elem = Free of Smt.Var.t | Fixed of bool [@@deriving sexp]

  type t = elem list [@@deriving sexp]

  let free x = Free x

  let fixed x = Fixed x

  let to_expr = function Free v -> Smt.var v | Fixed x -> Smt.bool x

  let create ?(prefix = sprintf "b%d") () =
    Smt.(
      List.init (Lazy.force Global.n_bits) ~f:(fun b ->
          fresh_decl ~prefix:(prefix b) () >>| free)
      |> all)

  let of_abs ?(prefix = sprintf "b%d") x =
    Smt.(
      List.init (Lazy.force Global.n_bits) ~f:(fun b ->
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
      Sequence.map models ~f:(fun model ->
          Map.fold model ~init:abs ~f:(fun ~key:var ~data:value abs ->
              let idx = Map.find_exn bit_idx var in
              Option.value_exn (Abs.Bool_vector.add abs idx value)))
      |> Sequence.map ~f:Abs.bool_vector
      |> Sequence.to_list
      |> Set.of_list (module Abs)
    in
    print_s
      [%message
        "refine"
          (models : Smt.Model.t Sequence.t)
          (symb : t)
          (abs : Abs.Bool_vector.t)
          (refined_states : Set.M(Abs).t)
          [%here]];
    refined_states
end

let offsets_of_type t =
  (Set_once.get_exn Global.bench [%here]).ops
  |> List.filter_map ~f:(function
       | Op.Offset x when [%compare.equal: Type.offset_type] x.Op.type_ t ->
           Some x.offset
       | _ -> None)

module Offset = struct
  type t = { set : Smt.Var.t list; type_ : Type.offset_type } [@@deriving sexp]

  let n_offsets t = offsets_of_type t |> List.length

  let create ?(prefix = sprintf "o%d") t =
    let open Smt in
    let open Let_syntax in
    let%map set =
      n_offsets t
      |> List.init ~f:(fun b -> fresh_decl ~prefix:(prefix b) ())
      |> all
    in
    { set; type_ = t }

  let ( = ) x x' =
    Smt.(List.map2_exn x.set x'.set ~f:(fun v v' -> var v = var v') |> and_)

  let contained x by =
    List.map2_exn x.set (offsets_of_type x.type_) ~f:(fun in_set offset ->
        let is_in = Float.(by.Abs.Offset.lo <= offset && offset <= by.hi) in
        Smt.(var in_set = bool is_in))
    |> Smt.and_

  let index_of ~equal l x =
    List.find_mapi l ~f:(fun i x' -> if equal x x' then Some i else None)

  let split (abs : Abs.Offset.t) symb var =
    index_of ~equal:[%compare.equal: Smt.Var.t] symb.set var
    |> Option.map ~f:(fun idx ->
           let split_offset = List.nth_exn (offsets_of_type symb.type_) idx in
           ( { Abs.Offset.lo = abs.lo; hi = split_offset },
             { Abs.Offset.lo = split_offset; hi = abs.hi } ))

  let refine_single (abs : Abs.Offset.t) symb var value =
    split abs symb var |> Option.map ~f:(fun (f, t) -> if value then t else f)

  let refine models old_abs symb =
    Sequence.map models ~f:(fun model ->
        Map.fold ~init:old_abs
          ~f:(fun ~key:var ~data:value abs ->
            refine_single abs symb var value |> Option.value ~default:abs)
          model)
    |> Sequence.map ~f:Abs.offset |> Sequence.to_list
    |> Set.of_list (module Abs)
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

let create ?prefix =
  let open Smt.Monad_infix in
  function
  | Type.Vector -> Bool_vector.create ?prefix () >>| bool_vector
  | Type.Offset t -> Offset.create ?prefix t >>| offset

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

let contained x ~by =
  map_abs ~vector:Bool_vector.(contained) ~offset:Offset.(contained) x by

let bool_vector_3 f x x' =
  Smt.(f (to_bool_vector_exn x) (to_bool_vector_exn x') >>| bool_vector)

let union = bool_vector_3 Bool_vector.union

let inter = bool_vector_3 Bool_vector.inter

let sub = bool_vector_3 Bool_vector.sub

let filter_offsets (var : Offset.t) pred =
  List.map2_exn (offsets_of_type var.type_) var.set ~f:(fun offset in_set ->
      if pred offset then Some (Smt.var in_set) else None)
  |> List.filter_map ~f:Fun.id

let cylinder (c : Op.cylinder) l h =
  let l = to_offset_exn l and h = to_offset_exn h in
  let ret =
    (Set_once.get_exn Global.bench [%here]).input |> Array.to_list
    |> List.map ~f:(fun v ->
           let open Vector3 in
           let rot = inverse_rotate v c.theta in
           (* Check whether point is in radius. *)
           let in_radius =
             Float.(
               square (rot.y - c.y) + square (rot.z - c.z) < square c.radius)
           in
           (* Collect offsets that are above this point. *)
           let above = filter_offsets l (fun o -> Float.(rot.x < o)) in
           (* Collect offsets that are below this point. *)
           let below = filter_offsets h (fun o -> Float.(rot.x > o)) in
           (* Point is in if it is in the radius, none of the low offsets that are
              above it are selected, and none of the high offsets that are below
              it are selected *)
           Smt.(
             fresh_defn (bool in_radius && (not (or_ above)) && not (or_ below))
             >>| Bool_vector.free))
  in
  Smt.(all ret >>| bool_vector)

let cuboid (c : Op.cuboid) lx hx ly hy lz hz =
  let lx = to_offset_exn lx
  and hx = to_offset_exn hx
  and ly = to_offset_exn ly
  and hy = to_offset_exn hy
  and lz = to_offset_exn lz
  and hz = to_offset_exn hz in
  let ret =
    (Set_once.get_exn Global.bench [%here]).input |> Array.to_list
    |> List.map ~f:(fun v ->
           let open Vector3 in
           let rot = inverse_rotate v c.theta in
           let above_x = filter_offsets lx (fun o -> Float.(o > rot.x))
           and below_x = filter_offsets hx (fun o -> Float.(o < rot.x))
           and above_y = filter_offsets ly (fun o -> Float.(o > rot.y))
           and below_y = filter_offsets hy (fun o -> Float.(o < rot.y))
           and above_z = filter_offsets lz (fun o -> Float.(o > rot.z))
           and below_z = filter_offsets hz (fun o -> Float.(o < rot.z)) in
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

let sphere (s : Op.sphere) =
  let ret =
    (Set_once.get_exn Global.bench [%here]).input |> Array.to_list
    |> List.map ~f:(fun v ->
           Smt.(
             fresh_defn (bool Float.(Vector3.l2_dist s.center v <= s.radius))
             >>| Bool_vector.free))
  in
  Smt.(all ret >>| bool_vector)

let offset (o : Op.offset) =
  let open Smt.Let_syntax in
  let%map set =
    List.map (offsets_of_type o.type_) ~f:(fun offset ->
        Smt.fresh_defn (Smt.bool Float.(o.offset = offset)))
    |> Smt.all
  in
  Offset.{ type_ = o.type_; set } |> offset

let eval op args =
  let open Util in
  match op with
  | Op.Union -> apply2 union args
  | Inter -> apply2 inter args
  | Sub -> apply2 sub args
  | Cylinder c -> apply2 (cylinder c) args
  | Cuboid c -> apply6 (cuboid c) args
  | Sphere s -> sphere s
  | Offset o -> offset o

let refine ivars old_state var =
  map
    ~vector:(fun s ->
      Bool_vector.refine ivars (Abs.to_bool_vector_exn old_state) s)
    ~offset:(fun s -> Offset.refine ivars (Abs.to_offset_exn old_state) s)
    var
