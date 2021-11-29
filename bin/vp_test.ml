open! Core
open Staged_synth
open Cad_ext
open Core_bench
open Synth_utils

module P = struct
  type t = Value.t [@@deriving compare, hash, sexp]

  let n_dist = ref 0

  let dist (v : Value.t) (v' : Value.t) =
    Int.incr n_dist;
    match (v, v') with Scene s, Scene s' -> Scene.jaccard s s' | _ -> Float.infinity
end

let () =
  let query, reference = Sexp.load_sexp_conv_exn "query_reference.sexp" [%of_sexp: Value.t list * Value.t list] in
  print_s [%message (List.length query : int) (List.length reference : int)];

  let query_t, query_time = timed (fun () -> Vpt.create P.dist ~leaf_size:1 (`Good 10) query) in
  let ref_t, ref_time = timed (fun () -> Vpt.create P.dist ~leaf_size:1 `Random reference) in

  P.n_dist := 0;
  let range_t, range_time =
    timed (fun () ->
        (Vpt.range [@specialized]) P.dist 0.0 0.1 query_t ref_t (fun v ->
            ignore (Sys.opaque_identity v : Value.t * Value.t)))
  in
  print_s
    [%message
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
