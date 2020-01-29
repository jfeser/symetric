open! Core

module type S = sig
  type ctype

  type expr

  val elem_t : ctype

  val mk_type : ctype -> ctype

  val elem_type : ctype -> ctype

  module O : sig
    val ( = ) : expr -> expr -> expr
  end

  val length : expr -> expr

  val const : ctype -> expr array -> 'a

  val init : 'a -> expr -> ('b -> 'c) -> 'd

  val set : expr -> expr -> expr -> expr

  val get : expr -> expr -> expr

  val map : 'a -> expr -> f:'b -> 'c

  val map2 : 'a -> expr -> expr -> f:(expr -> expr -> 'b) -> 'c

  val sub : expr -> expr -> expr -> 'a

  val fold : expr -> init:expr -> f:('a -> expr -> 'b) -> 'c

  val iter : expr -> f:(expr -> 'a) -> 'b

  val of_sexp : 'a -> expr -> (expr -> 'b) -> 'c

  val sexp_of : 'a -> 'b -> 'c
end

module Array (C : Cstage_core.S) = struct
  module Int = Cstage_int.Int (C)
  open C

  let no_effect e = { e with eeffect = false }

  let elem_t = Univ_map.Key.create ~name:"elem_t" [%sexp_of: ctype]

  let mk_type e =
    Type.create ~name:(sprintf "std::vector<%s>" (Type.name e))
    |> Type.add_exn ~key:elem_t ~data:e

  let elem_type t = Univ_map.find_exn t elem_t

  module O = struct
    let ( = ) a a' = binop "(%s == %s)" Bool.type_ a a'
  end

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
