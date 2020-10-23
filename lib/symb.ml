open Ast

module Bool_vector = struct
  type t = Smt.Var.t list [@@deriving sexp]

  let create ?(prefix = sprintf "b%d") () =
    List.init (Lazy.force Global.n_bits) ~f:(fun b ->
        Smt.(fresh_decl ~prefix:(prefix b) ()))
    |> Smt.all

  let union x x' =
    Smt.(
      List.map2_exn x x' ~f:(fun v v' -> fresh_defn (var v || var v')) |> all)

  let inter x x' =
    Smt.(
      List.map2_exn x x' ~f:(fun v v' -> fresh_defn (var v || var v')) |> all)

  let sub x x' =
    Smt.(
      List.map2_exn x x' ~f:(fun v v' -> fresh_defn (var v && not (var v')))
      |> all)

  let ( = ) x x' =
    Smt.(List.map2_exn x x' ~f:(fun v v' -> var v = var v') |> and_)

  let contained x by =
    let open Smt in
    List.filter_mapi x ~f:(fun b v ->
        match Map.find by b with
        | Some x -> Some (bool x = var v)
        | None -> None)
    |> and_

  let refine bits states op var =
    print_s
      [%message
        "refine"
          (op : Op.t)
          (bits : Set.M(Smt.Var).t)
          (var : t)
          (states : Abs.Bool_vector.t list)
          [%here]];

    let vbits = Set.of_list (module Smt.Var) var in
    let rbits = Set.inter vbits bits in
    let bit_idx =
      List.mapi var ~f:(fun i b -> (b, i)) |> Map.of_alist_exn (module Smt.Var)
    in

    let refined_states =
      if Int.(Op.arity op = 0) then
        let conc = Conc.eval op [] |> Conc.to_bool_vector_exn in
        Set.fold rbits ~init:states ~f:(fun states bit ->
            let idx = Map.find_exn bit_idx bit in
            List.filter_map states ~f:(fun state ->
                Abs.Bool_vector.add state idx conc.(idx)))
      else
        Set.fold rbits ~init:states ~f:(fun states bit ->
            let idx = Map.find_exn bit_idx bit in
            List.concat_map states ~f:(fun state ->
                List.filter_map [ true; false ]
                  ~f:(Abs.Bool_vector.add state idx)))
    in
    refined_states
    |> List.dedup_and_sort ~compare:[%compare: Abs.Bool_vector.t]
    |> List.map ~f:Abs.bool_vector
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

  let refine ivars states op var =
    raise_s
      [%message
        "unimplemented"
          (ivars : Set.M(Smt.Var).t)
          (states : Abs.Offset.t list)
          (op : Op.t)
          (var : t)
          [%here]]
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
             fresh_defn (bool in_radius && (not (or_ above)) && not (or_ below))))
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
           and above_y = filter_offsets ly (fun o -> Float.(o > rot.x))
           and below_y = filter_offsets hy (fun o -> Float.(o < rot.x))
           and above_z = filter_offsets lz (fun o -> Float.(o > rot.x))
           and below_z = filter_offsets hz (fun o -> Float.(o < rot.x)) in
           Smt.(
             fresh_defn
               ( (not (or_ above_x))
               && (not (or_ below_x))
               && (not (or_ above_y))
               && (not (or_ below_y))
               && (not (or_ above_z))
               && not (or_ below_z) )))
  in
  Smt.(all ret >>| bool_vector)

let sphere (s : Op.sphere) =
  let ret =
    (Set_once.get_exn Global.bench [%here]).input |> Array.to_list
    |> List.map ~f:(fun v ->
           Smt.fresh_defn
             (Smt.bool Float.(Vector3.l2_dist s.center v <= s.radius)))
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

let refine ivars states op var =
  map
    ~vector:(fun v ->
      Bool_vector.refine ivars (List.map ~f:Abs.to_bool_vector_exn states) op v)
    ~offset:(fun v ->
      Offset.refine ivars (List.map ~f:Abs.to_offset_exn states) op v)
    var
