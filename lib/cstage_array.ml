open! Core
open Types

module type S = sig
  type 'a code

  type 'a ctype

  type 'a t

  val mk_type : 'a ctype -> 'a t ctype

  val elem_type : 'a t ctype -> 'a ctype

  module O : sig
    val ( = ) : 'a t code -> 'a t code -> bool code
  end

  val const : 'a t ctype -> 'a code Array.t -> 'a t code

  val get : 'a t code -> int code -> 'a code

  val set : 'a t code -> int code -> 'a code -> unit code

  val length : 'a t code -> int code

  val fold :
    'a t code -> init:'b code -> f:('b code -> 'a code -> 'b code) -> 'b code

  val iter : 'a t code -> f:('a code -> unit code) -> unit code

  val sub : 'a t code -> int code -> int code -> 'a t code

  val init : 'a t ctype -> int code -> (int code -> 'a code) -> 'a t code

  val map : 'b t ctype -> 'a t code -> f:('a code -> 'b code) -> 'b t code

  val map2 :
    'c t ctype ->
    'a t code ->
    'b t code ->
    f:('a code -> 'b code -> 'c code) ->
    'c t code

  val of_sexp : 'a t ctype -> sexp code -> (sexp code -> 'a code) -> 'a t code

  val sexp_of : 'a t code -> ('a code -> sexp code) -> sexp code
end

module type Base = sig
  type typ

  type expr

  val create : typ -> expr -> expr

  val length : expr -> expr

  val set : expr -> expr -> expr -> expr

  val get : expr -> expr -> expr
end

module type Derived = sig
  type typ

  type expr

  val const : typ -> expr array -> expr

  val init : typ -> expr -> (expr -> expr) -> expr

  val map : typ -> expr -> f:(expr -> expr) -> expr

  val map2 : typ -> expr -> expr -> f:(expr -> expr -> expr) -> expr

  val sub : expr -> expr -> expr -> expr

  val fold : expr -> init:expr -> f:(expr -> expr -> expr) -> expr

  val iter : expr -> f:(expr -> expr) -> expr

  val of_sexp : typ -> expr -> (expr -> expr) -> expr

  val sexp_of : expr -> (expr -> expr) -> expr

  module O : sig
    val ( = ) : expr -> expr -> expr
  end
end

module type S_ = sig
  type typ

  type expr

  include Base with type typ := typ and type expr := expr

  include Derived with type typ := typ and type expr := expr

  val mk_type : typ -> typ

  val elem_type : typ -> typ
end

module Derived
    (C : Cstage_core.S)
    (B : Base with type expr = C.expr and type typ = C.typ) :
  Derived with type typ := C.typ and type expr := C.expr = struct
  module Int = Cstage_int.Int (C)
  open C
  open B

  let no_effect e = { e with eeffect = false }

  let const t a =
    let a = Array.to_list a in
    ( let_ (create t (Int.int (List.length a))) @@ fun arr ->
      sseq [ List.mapi a ~f:(fun i -> set arr (Int.int i)) |> sseq; arr ] )
    |> no_effect |> with_comment "Array.const"

  let init t len f =
    ( let_ (create t len) @@ fun a ->
      sseq
        [
          for_ (Int.int 0) (Int.int 1) len (fun i -> set a i (genlet (f i))); a;
        ] )
    |> no_effect |> with_comment "Array.init"

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

  module O = struct
    let ( = ) a a' = binop "(%s == %s)" Bool.type_ a a'
  end
end

module Array (C : Cstage_core.S) = struct
  module Int = Cstage_int.Int (C)
  open C

  type 'a t

  type 'a code = 'a C.t

  type typ = C.typ

  type 'a ctype = 'a C.ctype

  let elem_t = Univ_map.Key.create ~name:"elem_t" [%sexp_of: typ]

  let mk_type e =
    Type.create ~name:(sprintf "std::vector<%s>" (Type.name e))
    |> Type.add_exn ~key:elem_t ~data:e

  let elem_type t =
    match Univ_map.find t elem_t with
    | Some et -> et
    | None -> Error.create "Not an array type." t [%sexp_of: typ] |> Error.raise

  module Base = struct
    type typ = C.typ

    type expr = C.expr

    let create t n = fresh_decl ~init:n t

    let length x = unop "((int)((%s).size()))" Int.type_ x

    let set a i x =
      eformat ~has_effect:true "0" unit_t "$(a)[$(i)] = $(x);"
        [ ("a", C a); ("i", C i); ("x", C x) ]

    let get a x =
      eformat "($(a)[$(x)])" (elem_type a.etype) "" [ ("a", C a); ("x", C x) ]
  end

  include (Base : Base with type typ := C.typ and type expr := C.expr)

  include Derived (C) (Base)
end

module ArenaArray (C : Cstage_core.S) = struct
  module Int = Cstage_int.Int (C)
  open C
  module A = Array (C)

  type 'a t

  type typ = C.typ

  type 'a ctype = 'a C.ctype

  type 'a code = 'a C.t

  (** Number of elements in the arena. *)
  let default_size = 500_000

  let no_effect e = { e with eeffect = false }

  let elem_k = Univ_map.Key.create ~name:"elem_t" [%sexp_of: typ]

  let arena_k = Univ_map.Key.create ~name:"arena" [%sexp_of: expr]

  let arena_offset_k = Univ_map.Key.create ~name:"arena_offset" [%sexp_of: expr]

  let mk_type e =
    let arena_type =
      Type.create
        ~name:(sprintf "std::array<%s, %d>" (Type.name e) default_size)
    in
    Type.create ~name:(sprintf "span<%s>" (Type.name e))
    |> Type.add_exn ~key:elem_k ~data:e
    |> Type.add_exn ~key:arena_k ~data:(fresh_global arena_type)
    |> Type.add_exn ~key:arena_offset_k ~data:(fresh_global Int.type_)

  let elem_type t = Univ_map.find_exn t elem_k

  let arena t = Univ_map.find_exn t arena_k

  let arena_offset t = Univ_map.find_exn t arena_offset_k

  module Base = struct
    type typ = C.typ

    type expr = C.expr

    let create t l =
      let ctx =
        [
          ("arena", C (arena t));
          ("len", C l);
          ("type", S (Type.name t));
          ("offset", C (arena_offset t));
        ]
      in
      let_ (eformat "($(type)){($(arena)).data() + $(offset), $(len)}" t "" ctx)
      @@ fun arr -> sseq [ eformat "0" unit_t "$(offset) += $(len);" ctx; arr ]

    let length x = unop "((int)((%s).len))" Int.type_ x

    let set a i x =
      eformat ~has_effect:true "0" unit_t "$(a).ptr[$(i)] = $(x);"
        [ ("a", C a); ("i", C i); ("x", C x) ]

    let get a x =
      eformat "($(a).ptr[$(x)])" (elem_type a.etype) ""
        [ ("a", C a); ("x", C x) ]
  end

  include (Base : Base with type typ := C.typ and type expr := C.expr)

  include Derived (C) (Base)

  let sub a start len =
    let open Int in
    let_ (length a) @@ fun n ->
    let_ (start + len) @@ fun end_ ->
    let_ (start |> min (n - int 1) |> max (int 0)) @@ fun start ->
    let_ (end_ |> min n |> max start) @@ fun end_ ->
    let_ (end_ - start) @@ fun len ->
    let t = type_of a in
    eformat "($(type)){$(span).ptr + $(start), $(span).len + $(len)}" t ""
      [
        ("type", S (Type.name t));
        ("span", C a);
        ("len", C len);
        ("start", C start);
      ]
end

module ReversibleArray
    (C : Cstage_core.S)
    (A : S_ with type expr = C.expr and type typ = C.typ) =
struct
  module Int = Cstage_int.Int (C)
  module Tuple = Cstage_tuple.Tuple (C)
  open C

  type 'a code = 'a C.t

  type typ = C.typ

  type 'a ctype = 'a C.ctype

  type 'a t

  let elem_k = Univ_map.Key.create ~name:"elem_t" [%sexp_of: typ]

  let arena_offset_k = Univ_map.Key.create ~name:"arena_offset" [%sexp_of: expr]

  let mk_type e = Tuple.mk_type (A.mk_type e) Bool.type_

  let elem_type t = Univ_map.find_exn t elem_k

  module Base = struct
    type typ = C.typ

    type expr = C.expr

    let create t l = Tuple.create (A.create t l) (C.Bool.bool false)

    let length x = A.length (Tuple.fst x)

    let set a i x =
      C.let_ (Tuple.fst a) @@ fun arr ->
      C.ite (Tuple.snd a)
        (fun () -> A.set arr Int.(A.length arr - i - int 1) x)
        (fun () -> A.set arr i x)

    let get a i =
      C.let_ (Tuple.fst a) @@ fun arr ->
      C.ite (Tuple.snd a)
        (fun () -> A.get arr Int.(A.length arr - i - int 1))
        (fun () -> A.get arr i)
  end

  include (Base : Base with type typ := C.typ and type expr := C.expr)

  include Derived (C) (Base)

  let reverse a = Tuple.create (Tuple.fst a) (Bool.not (Tuple.snd a))
end
