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

module Rule = struct
  type pat = (Op.t, int) Local_search.Pattern.t [@@deriving compare, sexp]

  type t = pat * pat [@@deriving compare, sexp]

  let apply x y = Local_search.Pattern.Apply (x, y)
end

let mean ~n iter = Iter.sumf iter /. n

let std ~n ~u iter =
  let sum = Iter.map (fun x -> Float.square (x -. u)) iter |> Iter.sumf in
  Float.(sqrt (1.0 /. (n -. 1.0) *. sum))

let push_pull_replicate ops =
  let repls = List.filter ops ~f:(fun op -> match Op.value op with Op.Replicate _ -> true | _ -> false) in
  let open Rule in
  List.concat_map [ Op.union; Op.inter ] ~f:(fun binary ->
      List.concat_map repls ~f:(fun r ->
          [
            (apply binary [ apply r [ Var 0 ]; Var 1 ], apply r [ apply binary [ Var 0; Var 1 ] ]);
            (apply binary [ Var 0; apply r [ Var 1 ] ], apply r [ apply binary [ Var 0; Var 1 ] ]);
          ]))

let mk_hard_term params size =
  let open Cad_gen_pattern in
  let other_ops = replicates @ [ Cad_op.union; Cad_op.inter ] in
  let shape_ops = List.take (List.permute shapes) 4 in
  let ops = shape_ops @ other_ops in
  let params =
    Dumb_params.set params Cad_params.bench
      { ops; input = { xmax; ymax }; output = Cad_conc.dummy; solution = None; filename = None }
  in

  let filler = new filler params in
  let search_state = filler#build_search_state in

  let value = List.hd_exn @@ List.permute @@ B.Search_state.search ~cost:size ~type_:Cad_type.output search_state in
  let term = B.Search_state.random_program_exn search_state value in
  (term, value)

let of_queue q f = Queue.iter q ~f

let update_distances eval patterns term target =
  let target_dist t' = Cad_conc.jaccard target (eval t') in
  List.iter patterns ~f:(fun (rule, distances) ->
      Iter.forever (fun () -> sample_step rule term)
      |> Iter.take 100 |> Iter.filter_map Fun.id
      |> Iter.iter (fun t' -> Queue.enqueue distances @@ target_dist t'))

let run ?(size = 6) ?(n_terms = 100) params =
  let params = Dumb_params.set params Baseline.max_cost size in
  let ops = Cad_gen_pattern.ops in
  let eval = Program.eval (Value.eval params) in

  let patterns =
    Local_search.Pattern.rename_patterns ops @ push_pull_replicate ops
    |> List.map ~f:(fun (r, r') -> if [%compare: Rule.pat] r r' <= 0 then (r, r') else (r', r))
    |> List.dedup_and_sort ~compare:[%compare: Rule.t]
  in

  let distances = List.map patterns ~f:(fun p -> (p, Queue.create ())) in
  for _ = 0 to n_terms do
    let term, value = mk_hard_term params size in
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

  print_s [%message (distances_summary : (Rule.t * float * float * float) list)]

(* let min_found = 400 in
 * let all_rules =
 *   Queue.of_list @@ List.permute
 *   @@ List.dedup_and_sort ~compare:[%compare: Rule.t]
 *   @@ List.map ~f:(fun (r, r') -> if [%compare: Rule.pat] r r' <= 0 then (r, r') else (r', r))
 *   @@ Local_search.Pattern.rename_patterns ops
 * in
 * let rec choose_rules last_found rules =
 *   match Queue.dequeue all_rules with
 *   | Some new_rule -> (
 *       try
 *         let found = ref 0 and tries = 1_000 in
 *         for _ = 0 to tries do
 *           let random = Option.value_exn (random_walk ~n:3 rules solution) in
 *           found := !found + if hill_climb ~n:100 rules target eval random then 1 else 0
 *         done;
 *         print_s
 *           [%message (last_found : int) (!found : int) (tries : int) (List.length rules : int) (new_rule : Rule.t)];
 * 
 *         if !found >= min_found then choose_rules !found (new_rule :: rules) else choose_rules last_found rules
 *       with No_proposal -> choose_rules last_found (new_rule :: rules))
 *   | None -> rules
 * in
 * ignore (choose_rules 0 [] : _) *)

let cli =
  let open Command.Let_syntax in
  Command.basic ~summary:""
  @@ [%map_open
       let params = Dumb_params.Spec.(cli @@ union [ Cad_conc.spec; spec ]) in
       fun () -> run params]

let () = Command.run cli
