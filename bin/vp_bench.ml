open! Core
open Staged_synth
open Cad_ext

let dist_calls = ref 0

let distance (v : Value.t) (v' : Value.t) =
  incr dist_calls;
  match (v, v') with Scene x, Scene x' -> Scene2d.jaccard x x' | _ -> Float.infinity

module B = Bst.Bisec_tree.Make (struct
  type t = Value.t

  let dist = distance
end)

let mk_hash w k =
  let mask = Bitarray.of_list @@ List.permute (List.init w ~f:(fun i -> i < k)) in
  fun v -> Bitarray.and_ mask v

let mk_hashes n w k =
  List.init n ~f:(fun _ -> (mk_hash w k, Hashtbl.create (module Bitarray)))

let insert hs b = List.iter hs ~f:(fun (h, t) -> Hashtbl.add_multi t ~key:(h b) ~data:b)

let lookup hs b =
  List.concat_map hs ~f:(fun (h, t) -> Hashtbl.find t (h b) |> Option.value ~default:[])
  |> List.dedup_and_sort ~compare:[%compare: Bitarray.t]

let () =
  let thresh = 0.2 in
  let reference, query =
    Sexp.load_sexp_conv_exn (Sys.get_argv ()).(1) [%of_sexp: Value.t list * Value.t list]
  in

  let reference_vp = Vpt.create distance (`Good 10) reference in
  let reference_bst = B.create 5 Bst.Bisec_tree.Two_bands (Array.of_list reference) in

  let reference_lhs = mk_hashes 100 (12 * 20) 20 in
  List.iter reference ~f:(function
    | Scene s -> insert reference_lhs @@ Scene2d.pixels s
    | _ -> ());

  let[@inline] lookup_list v =
    List.filter reference ~f:(fun v' -> Float.(distance v v' < thresh))
  in
  let[@inline] lookup_vp v =
    Vpt.neighbors distance v thresh reference_vp |> Iter.to_list
  in
  let[@inline] lookup_bst v = B.neighbors v thresh reference_bst in
  let[@inline] lookup_bst_closest v = B.nearest_neighbor v reference_bst in
  let[@inline] lookup_lhs = function
    | Value.Scene s -> lookup reference_lhs @@ Scene2d.pixels s
    | _ -> []
  in

  print_s [%message (List.length query : int) (List.length reference : int)];

  dist_calls := 0;
  let start = Time.now () in
  List.iter query ~f:(fun v -> ignore (lookup_list v : _));
  print_s [%message (Time.(diff (now ()) start) : Time.Span.t) (!dist_calls : int)];

  dist_calls := 0;
  let start = Time.now () in
  List.iter query ~f:(fun v -> ignore (Sys.opaque_identity (lookup_vp v) : _ list));
  print_s [%message (Time.(diff (now ()) start) : Time.Span.t) (!dist_calls : int)];

  dist_calls := 0;
  let start = Time.now () in
  List.iter query ~f:(fun v -> ignore (Sys.opaque_identity (lookup_bst v) : _ list));
  print_s [%message (Time.(diff (now ()) start) : Time.Span.t) (!dist_calls : int)];

  dist_calls := 0;
  let start = Time.now () in
  List.iter query ~f:(fun v -> ignore (Sys.opaque_identity (lookup_bst_closest v) : _));
  print_s [%message (Time.(diff (now ()) start) : Time.Span.t) (!dist_calls : int)];

  dist_calls := 0;
  let start = Time.now () in
  List.iter query ~f:(fun v -> ignore (Sys.opaque_identity (lookup_lhs v) : _));
  print_s [%message (Time.(diff (now ()) start) : Time.Span.t) (!dist_calls : int)]

(* dist_calls := 0; *)
(* let start = Time.now () in *)
(* ignore *)
(*   (Sys.opaque_identity *)
(*      (Vpt.range distance 0.0 thresh query_vp reference_vp |> Iter.to_list) *)
(*     : _ list); *)
(* print_s [%message (Time.(diff (now ()) start) : Time.Span.t) (!dist_calls : int)] *)
