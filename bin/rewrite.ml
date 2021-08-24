open Core
open Staged_synth
open Cad

include struct
  open Dumb_params

  let spec = Spec.create ~name:"rewrite" ()

  let bench = Spec.add spec @@ Param.create Cad_params.bench_param
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
end

let mean iter =
  let num = ref 0.0 and den = ref 0.0 in
  iter (fun x ->
      num := !num +. x;
      den := !den +. 1.0);
  !num /. !den

let run params benchs =
  let benchs = List.map benchs ~f:Bench.load in
  let bench = List.hd_exn benchs in
  let ops = Bench.ops bench in
  let eval = Program.eval (Value.eval params) in

  let avg_dist =
    Local_search.Pattern.rename_patterns ops
    |> List.map ~f:(fun (r, r') -> if [%compare: Rule.pat] r r' <= 0 then (r, r') else (r', r))
    |> List.dedup_and_sort ~compare:[%compare: Rule.t]
    |> List.map ~f:(fun rule ->
           let avg =
             Iter.of_list benchs
             |> Iter.map (fun b ->
                    let solution = Cad.Bench.solution_exn b in
                    let target = Bench.output b in
                    Iter.forever (fun () ->
                        Option.map (sample_step rule solution) ~f:(fun t' -> Cad_conc.jaccard target (eval t')))
                    |> Iter.take 100 |> Iter.filter_map Fun.id)
             |> Iter.concat |> mean
           in
           (rule, avg))
    |> List.sort ~compare:(fun (_, d) (_, d') -> [%compare: float] d d')
  in

  print_s [%message (avg_dist : (Rule.t * float) list)]

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
       let params = Dumb_params.Spec.(cli @@ union [ Cad_conc.spec; spec ])
       and benchs = anon @@ sequence ("benchs" %: string) in
       fun () -> run params benchs]

let () = Command.run cli
