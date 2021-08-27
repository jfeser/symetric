open Core
open Staged_synth
open Cad

include struct
  open Dumb_params

  let spec = Spec.create ~name:"rewrite" ()
end

let stochastic ?(n = 5) ~score ~propose t f =
  let rec loop i t v =
    if i < n then (
      f (t, v);

      let t' = propose t in
      let v' = score t' in
      let ratio = v /. v' in
      let accept = Random.float 1.0 in
      if Float.(accept < ratio) then loop (i + 1) t' v' else loop (i + 1) t v)
  in
  loop 0 t (score t)

let last iter =
  let x = ref None in
  iter (fun x' -> x := Some x');
  !x

exception No_proposal

let sample_step ((lhs, rhs) as rule) t =
  let sampler = Sample.Incremental.reservoir 1 in
  Local_search.Pattern.rewrite_all rule t sampler.add;
  Local_search.Pattern.rewrite_all (rhs, lhs) t sampler.add;
  List.hd @@ sampler.get_sample ()

let all_rewrites ((lhs, rhs) as rule) t k =
  Local_search.Pattern.rewrite_all rule t k;
  Local_search.Pattern.rewrite_all (rhs, lhs) t k

let propose rules t =
  match List.permute rules |> List.find_map ~f:(fun r -> sample_step r t) with
  | Some t' -> t'
  | None -> raise No_proposal

let random_walk ?n rules t =
  let score _ = 1.0 in
  stochastic ?n ~score ~propose:(propose rules) t |> last |> Option.map ~f:(fun (p, _) -> p)

let hill_climb ?n rules target eval t =
  let score t' = Cad_conc.jaccard target @@ eval t' in
  let exception Done in
  let propose t' = if [%compare.equal: Value.t] (eval t') target then raise Done else propose rules t' in
  try
    stochastic ?n ~score ~propose t ignore;
    false
  with Done -> true

let mean ~n iter = Iter.sumf iter /. n

let std ~n ~u iter =
  let sum = Iter.map (fun x -> Float.square (x -. u)) iter |> Iter.sumf in
  Float.(sqrt (1.0 /. (n -. 1.0) *. sum))

module B = Baseline.Make (Cad)

class filler ctx =
  object (self)
    inherit B.synthesizer ctx

    method build_search_state =
      for cost = 0 to max_cost do
        self#fill cost
      done;
      search_state
  end

let mk_hard_term ectx size =
  let module Cgp = Cad_gen_pattern in
  let other_ops = Cgp.replicates @ [ Cad_op.union; Cad_op.inter ] in
  let shape_ops = List.take (List.permute Cgp.shapes) 4 in
  let ops = shape_ops @ other_ops in

  let filler = new filler (B.Ctx.create ~max_cost:size ectx ops Cad_conc.dummy ()) in
  let search_state = filler#build_search_state in

  let value = List.hd_exn @@ List.permute @@ B.Search_state.search ~cost:size ~type_:Cad_type.output search_state in
  let term = B.Search_state.random_program_exn search_state value in
  (term, value)

let of_queue q f = Queue.iter q ~f

let update_distances eval patterns term target =
  let target_dist t' = Cad_conc.jaccard target (eval t') in
  List.iter patterns ~f:(fun (rule, distances) ->
      all_rewrites rule term
      (* |> Iter.map (fun t' ->
       *        let v' = eval t' in
       *        if Float.(Random.float 1.0 < 0.01) then
       *          Fmt.epr "Orig:\n%a\nTransformed:\n%a\n\n%!" Cad_conc.pprint target Cad_conc.pprint v';
       *        t') *)
      |> Iter.map target_dist
      |> Iter.filter (fun d -> Float.(d > 0.0))
      |> Iter.max ~lt:Float.( < )
      |> Option.iter ~f:(Queue.enqueue distances))

let run ?(size = 6) ?(n_terms = 10000) () =
  let ops = Cad_gen_pattern.ops in
  let ectx = Value.Ctx.create ~xlen:Cad_gen_pattern.xmax ~ylen:Cad_gen_pattern.ymax () in
  let eval = Program.eval (Value.eval ectx) in

  let patterns =
    let open Local_search in
    Pattern.(rename_patterns ops @ push_pull_replicate ops)
    |> List.map ~f:(fun (r, r') -> if [%compare: Rule.pat] r r' <= 0 then (r, r') else (r', r))
    |> List.dedup_and_sort ~compare:[%compare: Rule.t]
  in

  let distances = List.map patterns ~f:(fun p -> (p, Queue.create ())) in
  for i = 1 to n_terms do
    Fmt.epr "Term %d/%d\n%!" i n_terms;
    let term, value = mk_hard_term ectx size in
    update_distances eval distances term value
  done;

  let distances_summary =
    List.map distances ~f:(fun (p, dists) ->
        let n = Float.of_int @@ Queue.length dists in
        let mean = mean ~n @@ of_queue dists in
        let std = std ~u:mean ~n @@ of_queue dists in
        (p, mean, std, n))
    |> List.sort ~compare:(fun (_, d, _, _) (_, d', _, _) -> [%compare: float] d d')
  in

  print_s @@ [%sexp_of: (Local_search.Rule.t * float * float * float) list] distances_summary

let () = run ()
