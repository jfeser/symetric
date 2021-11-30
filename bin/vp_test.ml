open! Core
open Staged_synth
open Cad_ext
open Core_bench
open Synth_utils

module P = struct
  module T = struct
    type t = Value.t [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let n_dist = ref 0

  let dist (v : Value.t) (v' : Value.t) =
    Int.incr n_dist;
    match (v, v') with Scene s, Scene s' -> Scene.jaccard s s' | _ -> Float.infinity
end

let rec group thresh tree f =
  match tree with
  | Vpt.Empty -> ()
  | Leaf l -> Array.iter l ~f:(fun p -> f (p, [ p ]))
  | Node n ->
      if Float.(n.lb_high < thresh) then
        let members = Vpt.iter n.left |> Iter.to_list in
        f (n.vp, members)
      else group thresh n.left f;
      group thresh n.right f

let with_tree query ref_t =
  timed (fun () ->
      List.fold query ~init:(ref_t, []) ~f:(fun (kept_t, kept) v ->
          let f1 = Vpt.neighbors P.dist v 0.1 ref_t |> Iter.to_list |> List.is_empty |> not in
          let f2 = List.map kept ~f:(fun v' -> Float.(P.dist v v' <= 0.1)) |> List.exists ~f:Fun.id in
          if f1 || f2 then (kept_t, kept) else (kept_t, v :: kept)))

let () =
  let query, reference = Sexp.load_sexp_conv_exn "query_reference.sexp" [%of_sexp: Value.t list * Value.t list] in
  let query = Set.to_list @@ Set.diff (Set.of_list (module P) query) (Set.of_list (module P) reference) in

  print_s [%message (List.length query : int) (List.length reference : int)];

  let query_t, query_time = timed (fun () -> Vpt.create P.dist ~leaf_size:1 (`Good 10) query) in
  let ref_t, ref_time = timed (fun () -> Vpt.create P.dist ~leaf_size:1 `Random reference) in

  P.n_dist := 0;
  let range_t, range_time =
    timed (fun () ->
        (Vpt.range [@specialized]) P.dist 0.0 0.1 query_t ref_t (fun v ->
            ignore (Sys.opaque_identity v : Value.t * Value.t)))
  in

  let brute_groups, brute_time =
    timed (fun () ->
        query
        |> List.fold ~init:reference ~f:(fun kept v ->
               if List.map kept ~f:(fun v' -> Float.(P.dist v v' < 0.1)) |> List.exists ~f:Fun.id then kept
               else v :: kept))
  in

  let (_, with_tree_groups), with_tree_time = with_tree query ref_t in

  let query_groups = group 0.1 query_t |> Iter.length in
  print_s
    [%message
      (query_groups : int)
        (List.length brute_groups : int)
        (brute_time : Time.Span.t)
        (Vpt.length ref_t + List.length with_tree_groups : int)
        (with_tree_time : Time.Span.t)
        (!P.n_dist : int)
        (List.length query * List.length reference : int)
        (query_time : Core_kernel_private.Span_float.t)
        (ref_time : Core_kernel_private.Span_float.t)
        (range_time : Time.Span.t)]
(* Command.run *)
(*   (Bench.make_command *)
(*      [ *)
(*        Bench.Test.create ~name:"create-query" (fun () -> Vp.create `Random query); *)
(*        Bench.Test.create ~name:"create-reference" (fun () -> Vp.create `Random reference); *)
(*      ]) *)
