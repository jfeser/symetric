open Core
open Staged_synth
open Cad
open Std

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
  Local_search.Pattern.rewrite_all (module Op) rule t sampler.add;
  Local_search.Pattern.rewrite_all (module Op) (rhs, lhs) t sampler.add;
  List.hd @@ sampler.get_sample ()

let all_rewrites ((lhs, rhs) as rule) t k =
  Local_search.Pattern.rewrite_all (module Op) rule t k;
  Local_search.Pattern.rewrite_all (module Op) (rhs, lhs) t k

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

  let filler = new filler (B.Ctx.create ~max_cost:size ectx ops @@ `Value Cad_conc.dummy) in
  let search_state = filler#build_search_state in

  let value = List.hd_exn @@ List.permute @@ B.Search_state.search ~cost:size ~type_:Cad_type.output search_state in
  let term = B.Search_state.random_program_exn search_state Scene value in
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
    Pattern.(rename_patterns (module Op) ops @ push_pull_replicate ops)
    |> List.map ~f:(fun (r, r') -> if [%compare: Op.t Rule.pat] r r' <= 0 then (r, r') else (r', r))
    |> List.dedup_and_sort ~compare:[%compare: Op.t Rule.t]
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

  print_s @@ [%sexp_of: (Op.t Local_search.Rule.t * float * float * float) list] distances_summary

let distance_graph ?(size = 10) () =
  let module G = Graph.Imperative.Graph.Concrete (Value) in
  let module Weight = struct
    type edge = G.E.t

    type t = int

    let weight _ = 1

    let compare = compare

    let add = ( + )

    let zero = 0
  end in
  let module Comp = Graph.Components.Undirected (G) in
  let module Path = Graph.Path.Dijkstra (G) (Weight) in
  let ops = Cad_gen_pattern.(shapes @ [ Cad_op.union; Cad_op.inter ]) in
  let ectx = Value.Ctx.create ~xlen:Cad_gen_pattern.xmax ~ylen:Cad_gen_pattern.ymax () in
  let eval = Program.eval (Value.eval ectx) in

  let search_state =
    let filler = new filler (B.Ctx.create ~max_cost:size ectx ops @@ `Value Cad_conc.dummy) in
    filler#build_search_state
  in
  let rules =
    let open Local_search in
    Pattern.(close_leaf_patterns ops)
    |> List.map ~f:(fun (r, r') -> if [%compare: Op.t Rule.pat] r r' <= 0 then (r, r') else (r', r))
    |> List.dedup_and_sort ~compare:[%compare: Op.t Rule.t]
  in
  print_s [%message (List.length rules : int)];

  let states = Array.of_list @@ B.Search_state.states search_state in
  let n_states = Array.length states in
  let random_state () = states.(Random.int n_states).value in

  print_endline "Generating transform graph.";
  let g = G.create () in
  Array.iter states ~f:(fun s -> G.add_vertex g s.value);
  Array.iter states ~f:(fun s ->
      let p = B.Search_state.program_exn search_state s.type_ s.value in
      List.iter rules ~f:(fun rule -> all_rewrites rule p |> Iter.iter (fun p' -> G.add_edge g s.value (eval p'))));
  print_s [%message (G.nb_vertex g : int) (G.nb_edges g : int)];

  print_endline "Calculating correlation.";

  let jvdist, fvdist, tdist =
    Iter.init (fun i ->
        if i mod 10 = 0 then print_s [%message (i : int)];
        let v = random_state () and v' = random_state () in
        let jvd = Cad_conc.jaccard v v' and fvd = Cad_conc.feature_dist v v' in
        let td =
          try
            let _, td = Path.shortest_path g v v' in
            Float.of_int td
          with Caml.Not_found -> Float.infinity
        in
        (jvd, fvd, td))
    |> Iter.take 100 |> Iter.to_list |> List.unzip3
  in

  let jvdist = Array.of_list jvdist and fvdist = Array.of_list fvdist and tdist = Array.of_list tdist in
  let open Owl.Stats in
  print_s
    [%message
      (min tdist : float)
        (max tdist : float)
        (median tdist : float)
        (min jvdist : float)
        (max jvdist : float)
        (median jvdist : float)
        (min fvdist : float)
        (max fvdist : float)
        (median fvdist : float)
        (corrcoef jvdist tdist : float)
        (kendall_tau jvdist tdist : float)
        (corrcoef fvdist tdist : float)
        (kendall_tau fvdist tdist : float)]

module G = Graph.Imperative.Digraph.Concrete (Value)

module Weight = struct
  type edge = G.E.t

  type t = int

  let weight _ = 1

  let compare = compare

  let add = ( + )

  let zero = 0
end

module Path = Graph.Path.Dijkstra (G) (Weight)

let weight_of_dist dist =
  (module struct
    type edge = G.E.t

    type t = float

    let weight (v, v') =
      let d = dist v and d' = dist v' in
      let w = if Float.(d >= d') then 1.0 else Float.infinity in
      print_s [%message (w : float)];
      w

    let compare = Float.compare

    let add = ( +. )

    let zero = 0.0
  end : Graph.Sig.WEIGHT
    with type edge = G.E.t
     and type t = float)

let stochastic ?(max_steps = 100) ~score ~propose t f =
  let rec loop i t v =
    if i < max_steps then (
      f (t, v);

      let t' = propose t in
      let v' = score t' in
      let ratio = v /. v' in
      let accept = Random.float 1.0 in
      (* print_s [%message (v : float) (v' : float) (ratio : float) (accept : float)]; *)
      if Float.(accept < ratio) then loop (i + 1) t' v' else loop (i + 1) t v)
  in
  loop 0 t (score t)

let sample_distance g dist start end_ =
  let propose v = G.succ g v |> Random.list_elem_exn in
  Iter.forever (fun () ->
      stochastic ~propose ~score:(fun p -> dist end_ p) start
      |> Iter.findi (fun i (p, _) ->
             let _, true_dist = Path.shortest_path g p end_ in
             print_s [%message (i : int) (true_dist : int)];
             if [%compare.equal: Cad_conc.t] p end_ then Some i else None))
  |> Iter.take 100
  |> Iter.map (Option.map ~f:Float.of_int)
  |> Iter.map (Option.value ~default:Float.nan)
  |> Iter.min_exn

let tabu ?(max_tabu = 10) ~neighbors start k =
  let seen = Hash_queue.create @@ Base.Hashable.of_key (module Cad_conc) in
  let current = ref start in
  Hash_queue.enqueue_back_exn seen start ();
  while true do
    let next = Iter.find_pred_exn (fun c -> not (Hash_queue.mem seen c)) (neighbors !current) in
    Hash_queue.enqueue_back_exn seen next ();
    current := next;
    if Hash_queue.length seen > max_tabu then Hash_queue.drop_front seen;
    k next
  done

let sample_distance_tabu g dist start end_ =
  let neighbors v =
    G.succ g v
    |> List.map ~f:(fun v' -> (dist v' end_, v'))
    |> List.sort ~compare:(fun (d, _) (d', _) -> [%compare: float] d d')
    |> List.map ~f:(fun (_, v) -> v)
    |> Iter.of_list
  in
  Iter.forever (fun () ->
      tabu ~neighbors start |> Iter.take 10
      |> Iter.findi (fun i p ->
             (* let _, true_dist = Path.shortest_path g p end_ in
              * print_s [%message (i : int) (true_dist : int)]; *)
             if [%compare.equal: Cad_conc.t] p end_ then Some i else None))
  |> Iter.take 10
  |> Iter.map (Option.map ~f:Float.of_int)
  |> Iter.map (Option.value ~default:Float.nan)
  |> Iter.min_exn

let stochastic_distance ?(size = 10) () =
  let ops = Cad_gen_pattern.(shapes @ [ Cad_op.union; Cad_op.inter ]) in
  let ectx = Value.Ctx.create ~xlen:Cad_gen_pattern.xmax ~ylen:Cad_gen_pattern.ymax () in
  let eval = Program.eval (Value.eval ectx) in

  let search_state =
    let filler = new filler (B.Ctx.create ~max_cost:size ectx ops @@ `Value Cad_conc.dummy) in
    filler#build_search_state
  in
  let rules =
    let open Local_search in
    Pattern.(close_leaf_patterns ops)
    |> List.map ~f:(fun (r, r') -> if [%compare: Op.t Rule.pat] r r' <= 0 then (r, r') else (r', r))
    |> List.dedup_and_sort ~compare:[%compare: Op.t Rule.t]
  in
  print_s [%message (List.length rules : int)];

  let states = Array.of_list @@ B.Search_state.states search_state in
  let n_states = Array.length states in
  let random_state () = states.(Random.int n_states).value in

  print_endline "Generating transform graph.";
  let g = G.create () in
  Array.iter states ~f:(fun s -> G.add_vertex g s.value);
  Array.iter states ~f:(fun { value = v; type_ = t } ->
      let p = B.Search_state.program_exn search_state t v in
      List.iter rules ~f:(fun rule ->
          all_rewrites rule p
          |> Iter.iter (fun p' ->
                 let v' = eval p' in
                 G.add_edge g v v';
                 G.add_edge g v' v)));
  print_s [%message (G.nb_vertex g : int) (G.nb_edges g : int)];

  print_endline "Calculating correlation.";

  let jvdist, fvdist, jtdist, ftdist =
    Iter.init (fun i ->
        if i mod 10 = 0 then print_s [%message (i : int)];
        let v = random_state () and v' = random_state () in
        let jvd = Cad_conc.jaccard v v' and fvd = Cad_conc.feature_dist v v' in

        let jtd = sample_distance_tabu g Cad_conc.jaccard v v' in

        let ftd = sample_distance_tabu g Cad_conc.feature_dist v v' in
        (jvd, fvd, jtd, ftd))
    |> Iter.take 10 |> Iter.to_list |> List.unzip4
  in

  let jvdist = Array.of_list jvdist
  and fvdist = Array.of_list fvdist
  and jtdist = Array.of_list jtdist
  and ftdist = Array.of_list ftdist in
  let open Owl.Stats in
  print_s
    [%message
      (min ftdist : float)
        (max ftdist : float)
        (median ftdist : float)
        (min jtdist : float)
        (max jtdist : float)
        (median jtdist : float)
        (min jvdist : float)
        (max jvdist : float)
        (median jvdist : float)
        (min fvdist : float)
        (max fvdist : float)
        (median fvdist : float)
        (corrcoef jvdist jtdist : float)
        (kendall_tau jvdist jtdist : float)
        (corrcoef fvdist ftdist : float)
        (kendall_tau fvdist ftdist : float)]

let () = stochastic_distance ()
