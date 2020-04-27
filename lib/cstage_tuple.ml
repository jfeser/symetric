open! Core
open Types

module Tuple (C : Cstage_core.S) = struct
  module Int = Cstage_int.Int (C)
  open C

  let fst_t = Univ_map.Key.create ~name:"fst_t" [%sexp_of: typ]

  let snd_t = Univ_map.Key.create ~name:"snd_t" [%sexp_of: typ]

  let mk_type x y =
    Type.create ~name:(sprintf "std::pair<%s,%s >" (Type.name x) (Type.name y))
    |> Type.add_exn ~key:fst_t ~data:x
    |> Type.add_exn ~key:snd_t ~data:y

  let create x y =
    let type_ = mk_type x.etype y.etype in
    eformat "std::make_pair($(x), $(y))" type_ "" [ ("x", C x); ("y", C y) ]

  let of_sexp x t1_of_sexp t2_of_sexp =
    let_ (Sexp.to_list x) @@ fun x ->
    create
      (t1_of_sexp @@ Sexp.List.get x (Int.int 0))
      (t2_of_sexp @@ Sexp.List.get x (Int.int 1))

  let fst_type t = Univ_map.find_exn t fst_t

  let snd_type t = Univ_map.find_exn t snd_t

  let fst t = eformat "std::get<0>($(t))" (fst_type t.etype) "" [ ("t", C t) ]

  let snd t = eformat "std::get<1>($(t))" (snd_type t.etype) "" [ ("t", C t) ]

  let of_tuple (x, y) = create x y

  let to_tuple t = (fst t, snd t)

  let sexp_of _ _ _ = failwith "unimplemented"
end

module Tuple_3 = struct
  module type S = sig
    type ('a, 'b, 'c) t

    type 'a ctype

    type 'a code

    val mk_type : 'a ctype -> 'b ctype -> 'c ctype -> ('a, 'b, 'c) t ctype

    val fst : ('a, 'b, 'c) t code -> 'a code

    val of_tuple : 'a code * 'b code * 'c code -> ('a, 'b, 'c) t code

    val tuple_of :
      ('a, 'b, 'c) t code -> ('a code * 'b code * 'c code -> 'd code) -> 'd code

    val of_sexp :
      sexp code ->
      (sexp code -> 'a code) ->
      (sexp code -> 'b code) ->
      (sexp code -> 'c code) ->
      ('a, 'b, 'c) t code

    val sexp_of :
      ('a, 'b, 'c) t code ->
      ('a code -> sexp code) ->
      ('b code -> sexp code) ->
      ('c code -> sexp code) ->
      sexp code
  end

  module Make (C : Cstage_core.S) = struct
    module Int = Cstage_int.Int (C)
    open C

    type ('a, 'b, 'c) t

    let fst_t = Univ_map.Key.create ~name:"fst_t" [%sexp_of: typ]

    let snd_t = Univ_map.Key.create ~name:"snd_t" [%sexp_of: typ]

    let thd_t = Univ_map.Key.create ~name:"thd_t" [%sexp_of: typ]

    let mk_type x y z =
      Type.create
        ~name:
          (sprintf "std::tuple<%s,%s,%s >" (Type.name x) (Type.name y)
             (Type.name z))
      |> Type.add_exn ~key:fst_t ~data:x
      |> Type.add_exn ~key:snd_t ~data:y
      |> Type.add_exn ~key:thd_t ~data:z

    let create x y z =
      let type_ = mk_type x.etype y.etype z.etype in
      eformat "std::make_tuple($(x), $(y), $(z))" type_ ""
        [ ("x", C x); ("y", C y); ("z", C z) ]

    let of_sexp x t1_of_sexp t2_of_sexp t3_of_sexp =
      let_ (Sexp.to_list x) @@ fun x ->
      create
        (t1_of_sexp @@ Sexp.List.get x (Int.int 0))
        (t2_of_sexp @@ Sexp.List.get x (Int.int 1))
        (t3_of_sexp @@ Sexp.List.get x (Int.int 2))

    let fst_type t = Univ_map.find_exn t fst_t

    let snd_type t = Univ_map.find_exn t snd_t

    let thd_type t = Univ_map.find_exn t thd_t

    let fst t = eformat "std::get<0>($(t))" (fst_type t.etype) "" [ ("t", C t) ]

    let snd t = eformat "std::get<1>($(t))" (snd_type t.etype) "" [ ("t", C t) ]

    let thd t = eformat "std::get<2>($(t))" (thd_type t.etype) "" [ ("t", C t) ]

    let sexp_of _ _ _ = failwith "unimplemented"

    let of_tuple (a, b, c) = create a b c

    let tuple_of t f =
      C.let_ t @@ fun t ->
      C.let_ (fst t) @@ fun x1 ->
      C.let_ (snd t) @@ fun x2 ->
      C.let_ (thd t) @@ fun x3 -> f (x1, x2, x3)
  end
end

module Tuple_4 = struct
  module type S = sig
    type ('a, 'b, 'c, 'd) t

    type 'a ctype

    type 'a code

    val mk_type :
      'a ctype -> 'b ctype -> 'c ctype -> 'd ctype -> ('a, 'b, 'c, 'd) t ctype

    val of_tuple :
      'a code * 'b code * 'c code * 'd code -> ('a, 'b, 'c, 'd) t code

    val tuple_of :
      ('a, 'b, 'c, 'd) t code ->
      ('a code * 'b code * 'c code * 'd code -> 'e code) ->
      'e code

    val of_sexp :
      sexp code ->
      (sexp code -> 'a code) ->
      (sexp code -> 'b code) ->
      (sexp code -> 'c code) ->
      (sexp code -> 'd code) ->
      ('a, 'b, 'c, 'd) t code

    val sexp_of :
      ('a, 'b, 'c, 'd) t code ->
      ('a code -> sexp code) ->
      ('b code -> sexp code) ->
      ('c code -> sexp code) ->
      ('d code -> sexp code) ->
      sexp code

    val fst : ('a, 'b, 'c, 'd) t code -> 'a code
  end

  module Make (C : Cstage_core.S) = struct
    module Int = Cstage_int.Int (C)
    open C

    type ('a, 'b, 'c, 'd) t

    let fst_t = Univ_map.Key.create ~name:"fst_t" [%sexp_of: typ]

    let snd_t = Univ_map.Key.create ~name:"snd_t" [%sexp_of: typ]

    let thd_t = Univ_map.Key.create ~name:"thd_t" [%sexp_of: typ]

    let fth_t = Univ_map.Key.create ~name:"fth_t" [%sexp_of: typ]

    let mk_type x y z a =
      Type.create
        ~name:
          (sprintf "std::tuple<%s,%s,%s,%s >" (Type.name x) (Type.name y)
             (Type.name z) (Type.name a))
      |> Type.add_exn ~key:fst_t ~data:x
      |> Type.add_exn ~key:snd_t ~data:y
      |> Type.add_exn ~key:thd_t ~data:z
      |> Type.add_exn ~key:fth_t ~data:a

    let create x y z a =
      let type_ = mk_type x.etype y.etype z.etype a.etype in
      eformat "std::make_tuple($(x), $(y), $(z), $(a))" type_ ""
        [ ("x", C x); ("y", C y); ("z", C z); ("a", C a) ]

    let of_sexp x t1_of_sexp t2_of_sexp t3_of_sexp t4_of_sexp =
      let_ (Sexp.to_list x) @@ fun x ->
      create
        (t1_of_sexp @@ Sexp.List.get x (Int.int 0))
        (t2_of_sexp @@ Sexp.List.get x (Int.int 1))
        (t3_of_sexp @@ Sexp.List.get x (Int.int 2))
        (t4_of_sexp @@ Sexp.List.get x (Int.int 3))

    let fst_type t = Univ_map.find_exn t fst_t

    let snd_type t = Univ_map.find_exn t snd_t

    let thd_type t = Univ_map.find_exn t thd_t

    let fth_type t = Univ_map.find_exn t fth_t

    let fst t = eformat "std::get<0>($(t))" (fst_type t.etype) "" [ ("t", C t) ]

    let snd t = eformat "std::get<1>($(t))" (snd_type t.etype) "" [ ("t", C t) ]

    let thd t = eformat "std::get<2>($(t))" (thd_type t.etype) "" [ ("t", C t) ]

    let fth t = eformat "std::get<3>($(t))" (fth_type t.etype) "" [ ("t", C t) ]

    let of_tuple (a, b, c, d) = create a b c d

    let tuple_of t f =
      C.let_ t @@ fun t ->
      C.let_ (fst t) @@ fun x1 ->
      C.let_ (snd t) @@ fun x2 ->
      C.let_ (thd t) @@ fun x3 ->
      C.let_ (fth t) @@ fun x4 -> f (x1, x2, x3, x4)

    let sexp_of _ _ _ = failwith "unimplemented"
  end
end
