open! Core
open Types

module type S = sig
  type 'a t

  type 'a code

  type 'a ctype

  val mk_type : 'a ctype -> 'a set ctype

  val empty : 'a set ctype -> 'a set code

  val add : 'a set code -> 'a code -> unit code

  val iter : 'a set code -> ('a code -> unit code) -> unit code

  val fold :
    'a set code -> init:'b code -> f:('b code -> 'a code -> 'b code) -> 'b code

  val of_sexp :
    'a set ctype -> sexp code -> (sexp code -> 'a code) -> 'a set code

  val sexp_of : 'a set code -> ('a code -> sexp code) -> sexp code
end

module Set (C : Cstage_core.S) = struct
  module Int = Cstage_int.Int (C)
  open C

  type 'a t

  let elem_t = Univ_map.Key.create ~name:"elem_t" [%sexp_of: typ]

  let elem_type t = Univ_map.find_exn t elem_t

  let empty typ =
    let set = fresh_name () in
    add_local { vname = set; vtype = typ; init = None };
    { ret = set; ebody = ""; etype = typ; efree = []; eeffect = true }

  let iter a f =
    let iter_name = fresh_name () in
    let_locus @@ fun () ->
    let body =
      with_stackmark (fun m ->
          let arg =
            {
              ret = sprintf "*%s" iter_name;
              ebody = "";
              etype = elem_type a.etype;
              efree = [ (iter_name, m) ];
              eeffect = false;
            }
          in
          let_locus @@ fun () -> f arg)
    in
    eformat "0" unit_t
      {|
for(auto $(iter) = $(set).begin(); $(iter) != $(set).end(); ++$(iter)) {
        $(body)
}
|}
      [ ("set", C a); ("iter", S iter_name); ("body", S body.ebody) ]

  let fold a ~init ~f =
    let_ (fresh_global init.etype) (fun acc ->
        sseq
          [
            assign init ~to_:acc;
            iter a (fun x -> assign (f acc x) ~to_:acc);
            acc;
          ])
    |> with_comment "Set.fold"

  let add a x =
    eformat ~has_effect:true "0" unit_t "$(name).insert($(val));"
      [ ("name", C a); ("val", C x) ]

  let mk_type e =
    Type.create ~name:(sprintf "std::set<%s >" (Type.name e))
    |> Type.add_exn ~key:elem_t ~data:e

  let of_sexp type_ sexp elem_of_sexp =
    let_ (empty type_) @@ fun set ->
    let_ (Sexp.to_list sexp) @@ fun sexp ->
    for_ (Int.int 0) (Int.int 1) (Sexp.List.length sexp) (fun i ->
        add set (elem_of_sexp (Sexp.List.get sexp i)))

  let sexp_of _ _ = failwith "unimplemented"
end
