open! Core

module type Base = sig
  type typ

  type expr

  val length : expr -> expr

  val const : typ -> expr array -> expr

  val init : typ -> expr -> (expr -> expr) -> expr

  val set : expr -> expr -> expr -> expr

  val get : expr -> expr -> expr
end

module type Derived = sig
  type typ

  type expr

  val map : typ -> expr -> f:(expr -> expr) -> expr

  val map2 : typ -> expr -> expr -> f:(expr -> expr -> expr) -> expr

  val sub : expr -> expr -> expr -> expr

  val fold : expr -> init:expr -> f:(expr -> expr -> expr) -> expr

  val iter : expr -> f:(expr -> expr) -> expr

  val of_sexp : typ -> expr -> (expr -> expr) -> expr

  val sexp_of : expr -> (expr -> expr) -> expr
end

module type S = sig
  type typ

  type expr

  include Base with type typ := typ and type expr := expr

  include Derived with type typ := typ and type expr := expr

  val mk_type : typ -> typ

  val elem_type : typ -> typ

  module O : sig
    val ( = ) : expr -> expr -> expr
  end
end

module Derived
    (C : Cstage_core.S)
    (B : Base with type expr = C.expr and type typ = C.typ) :
  Derived with type typ := C.typ and type expr := C.expr = struct
  module Int = Cstage_int.Int (C)
  open C
  open B

  let map t arr ~f = init t (length arr) (fun i -> let_ (get arr i) f)

  let map2 t a1 a2 ~f =
    let_ (Int.min (length a1) (length a2)) @@ fun n ->
    init t n (fun i -> f (get a1 i) (get a2 i))

  let sub a start len =
    let open Int in
    let_ (length a) @@ fun n ->
    let_ (start + len) @@ fun end_ ->
    let_ (start |> min (n - int 1) |> max (int 0)) @@ fun start ->
    let_ (end_ |> min n |> max start) @@ fun end_ ->
    let_ (end_ - start) @@ fun len ->
    init a.etype len (fun i -> get a (start + i))

  let fold arr ~init ~f =
    ( let_ (fresh_decl (type_of init) ~init) @@ fun acc ->
      sseq
        [
          ( let_ (length arr) @@ fun len ->
            for_ (Int.int 0) (Int.int 1) len (fun i ->
                assign (f acc (get arr i)) ~to_:acc) );
          acc;
        ] )
    |> with_comment "Array.fold"

  let iter arr ~f =
    sseq
      [
        ( let_ (length arr) @@ fun len ->
          for_ (Int.int 0) (Int.int 1) len (fun i -> f (get arr i)) );
      ]
    |> with_comment "Array.iter"

  let of_sexp t x elem_of_sexp =
    let_ (Sexp.to_list x) @@ fun l ->
    init t (Sexp.List.length l) (fun i -> elem_of_sexp (Sexp.List.get l i))

  let sexp_of _ _ = failwith "unimplemented"
end

module Array (C : Cstage_core.S) = struct
  module Int = Cstage_int.Int (C)
  open C

  type 'a array = Array

  type 'a t = 'a C.t

  type typ = C.typ

  type 'a ctype = 'a C.ctype

  let no_effect e = { e with eeffect = false }

  let elem_t = Univ_map.Key.create ~name:"elem_t" [%sexp_of: typ]

  let mk_type e =
    Type.create ~name:(sprintf "std::vector<%s>" (Type.name e))
    |> Type.add_exn ~key:elem_t ~data:e

  let elem_type t =
    match Univ_map.find t elem_t with
    | Some et -> et
    | None -> Error.create "Not an array type." t [%sexp_of: typ] |> Error.raise

  module O = struct
    let ( = ) a a' = binop "(%s == %s)" Bool.type_ a a'
  end

  module Base = struct
    type typ = C.typ

    type expr = C.expr

    let length x = unop "((int)((%s).size()))" Int.type_ x

    let set a i x =
      eformat ~has_effect:true "0" unit_t "$(a)[$(i)] = $(x);"
        [ ("a", C a); ("i", C i); ("x", C x) ]

    let get a x =
      eformat "($(a)[$(x)])" (elem_type a.etype) "" [ ("a", C a); ("x", C x) ]

    let const t a =
      let a = Array.to_list a in
      ( let_ (fresh_decl ~init:(Int.int (List.length a)) t) @@ fun arr ->
        sseq [ List.mapi a ~f:(fun i -> set arr (Int.int i)) |> sseq; arr ] )
      |> no_effect |> with_comment "Array.const"

    let init t len f =
      ( let_ (fresh_decl ~init:len t) @@ fun a ->
        sseq
          [
            for_ (Int.int 0) (Int.int 1) len (fun i -> set a i (genlet (f i)));
            a;
          ] )
      |> no_effect |> with_comment "Array.init"
  end

  include (Base : Base with type typ := C.typ and type expr := C.expr)

  include Derived (C) (Base)
end

module ImmutableArray (C : Cstage_core.S) = struct
  module A = Array (C)

  let const = A.const

  let init = A.init

  let get = A.get

  let map = A.map

  let map2 = A.map2

  let sub = A.sub

  let fold = A.fold

  let iter = A.iter
end

(* module FixedSizeArray (Size : sig
 *   val size : int
 * end)
 * (C : Cstage_core.S) =
 * struct
 *   module Int = Cstage_int.Int (C)
 *   open C
 * 
 *   type 'a t = 'a C.t
 * 
 *   type typ = C.typ
 * 
 *   let no_effect e = { e with eeffect = false }
 * 
 *   let elem_t = Univ_map.Key.create ~name:"elem_t" [%sexp_of: typ]
 * 
 *   let mk_type e =
 *     Type.create
 *       ~name:(sprintf "std::span<std::array<%s, %d>>" (Type.name e) Size.size)
 *     |> Type.add_exn ~key:elem_t ~data:e
 * 
 *   let elem_type t = Univ_map.find_exn t elem_t
 * 
 *   module O = struct
 *     let ( = ) a a' = binop "(%s == %s)" Bool.type_ a a'
 *   end
 * 
 *   module Base = struct
 *     type typ = C.typ
 * 
 *     type expr = C.expr
 * 
 *     let length x = unop "((int)((%s).size()))" Int.type_ x
 * 
 *     let set a i x =
 *       eformat ~has_effect:true "0" unit_t "$(a)[$(i)] = $(x);"
 *         [ ("a", C a); ("i", C i); ("x", C x) ]
 * 
 *     let get a x =
 *       eformat "($(a)[$(x)])" (elem_type a.etype) "" [ ("a", C a); ("x", C x) ]
 * 
 *     let const t a =
 *       let a = Core.Array.to_list a in
 *       ( let_ (create t (Core.List.length a)) @@ fun arr ->
 *         sseq [ List.mapi a ~f:(fun i -> set arr (Int.int i)) |> sseq; arr ] )
 *       |> no_effect |> with_comment "Array.const"
 * 
 *     let init t len f =
 *       ( let_ (create t len) @@ fun a ->
 *         sseq
 *           [
 *             for_ (Int.int 0) (Int.int 1) len (fun i -> set a i (genlet (f i)));
 *             a;
 *           ] )
 *       |> no_effect |> with_comment "Array.init"
 *   end
 * 
 *   include (Base : Base with type typ := C.typ and type expr := C.expr)
 * 
 *   include Derived (C) (Base)
 * end *)
