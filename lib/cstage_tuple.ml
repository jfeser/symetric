open! Core

module Tuple (C : Cstage_core.S) = struct
  module Int = Cstage_int.Int (C)
  open C

  let fst_t = Univ_map.Key.create ~name:"fst_t" [%sexp_of: ctype]

  let snd_t = Univ_map.Key.create ~name:"snd_t" [%sexp_of: ctype]

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

  let sexp_of _ _ _ = failwith "unimplemented"
end
