include struct
  open Dumb_params

  let spec = Spec.inherit_ Baseline.spec "sampling-diverse"

  let retain_thresh =
    Spec.add spec
    @@ Param.float ~name:"retain-thresh" ~aliases:[ "r" ]
         ~doc:" retain points that are at least r away from their closest neighbor" ~init:(`Cli (Some 0.0)) ()

  let search_thresh =
    Spec.add spec
    @@ Param.float ~name:"search-thresh" ~aliases:[ "d" ] ~doc:" exhaustive search threshold" ~init:(`Cli (Some 1.0)) ()

  let ball_width =
    Spec.add spec @@ Param.int ~name:"width" ~aliases:[ "w" ] ~doc:" exhaustive search width" ~init:(`Cli (Some 2)) ()

  let diversity =
    Spec.add spec @@ Param.bool ~name:"diversity" ~doc:" use diversity sampling" ~init:(`Cli (Some true)) ()

  let final_value_dist = Spec.add spec @@ Param.float_ref ~name:"final-value-dist" ()

  let final_program_dist = Spec.add spec @@ Param.float_ref ~name:"final-program-dist" ()

  let (_ : _) = Spec.add spec @@ Param.const_str ~name:"synth" "sampling-diverse"

  let local_search =
    Spec.add spec
    @@ Param.symbol ~name:"local" ~doc:" kind of local search" ~default:`Bounded
         [ (`Bounded, "bounded"); (`Stochastic, "stochastic"); (`Leaf, "leaf") ]
end

include struct
  module Lang = Cad
  open Lang
  module Parent = Baseline.Make (Lang)
  module Search_state = Parent.Search_state

  let bounded_search params bench width =
    let ops = Bench.ops bench in

    let output = Bench.output bench in

    let search ?(view = ignore) _ center k =
      let check_program p =
        let v = Program.eval (Value.eval params) p in
        view v;
        if Value.equal v output then k p
      in
      try Tree_ball.Rename_insert_delete.ball (module Op) ops center width check_program
      with Program.Eval_error e -> raise @@ Program.Eval_error [%message (center : Op.t Program.t) (e : Sexp.t)]
    in
    search

  let stochastic_search params bench =
    let ops = Bench.ops bench and output = Bench.output bench in
    let search ?(view = ignore) synth center k =
      try
        Tree_ball.Rename_insert_delete.stochastic
          (module Op)
          ~score:(fun p -> 1.0 -. (synth#distance output @@ Program.eval (Value.eval params) p))
          ops center
          (fun p _ ->
            let v = Program.eval (Value.eval params) p in
            view v;
            if [%compare.equal: Value.t] v output then k p)
      with Program.Eval_error e -> raise @@ Program.Eval_error [%message (center : Op.t Program.t) (e : Sexp.t)]
    in
    search

  let leaf_search params bench =
    let module Op = Cad_op in
    let module Value = Cad_conc in
    let module F = Flat_program.Make (Op) in
    let eval = F.eval (Value.eval params) in
    let output = Bench.output bench in
    let search ?(view = ignore) synth center k =
      try
        Tree_ball.Rename_leaves.stochastic
          (module Op)
          Cad_gen_pattern.rename center ~n:100
          ~score:(fun p -> synth#distance output @@ eval p)
          (fun p _ ->
            let v = eval p in
            view v;
            if [%compare.equal: Value.t] v output then k @@ F.to_program p)
      with Program.Eval_error e -> raise @@ Program.Eval_error [%message (center : Op.t Program.t) (e : Sexp.t)]
    in
    search

  let sample_pairwise dist retain_thresh states old new_ =
    let min_dists =
      Dumb_progress.List.map ~name:"sampling" new_ ~f:(fun new_idx ->
          let min_dist =
            List.map old ~f:(fun old_idx -> dist states.(old_idx) states.(new_idx))
            |> List.min_elt ~compare:[%compare: float]
            |> Option.value ~default:Float.infinity
          in
          (min_dist, new_idx))
    in
    List.filter min_dists ~f:(fun (d, _) -> Float.(d >= retain_thresh)) |> List.map ~f:Tuple.T2.get2

  type stat = { mutable size : int }

  (* let classes =
   *   Search_state.states ss
   *   |> List.mapi ~f:(fun i v -> (v, Union_find.create i))
   *   |> Hashtbl.of_alist_exn (module Value)
   * in
   * let module F = Flat_program.Make (Op) in
   * let i = ref 0 in
   * Dumb_progress.(with_bar (basic_bar @@ Hashtbl.length classes)) @@ fun bar ->
   * Hashtbl.iteri classes ~f:(fun ~key:v ~data:c ->
   *     let cost = Option.value_exn (Search_state.cost_of ss v) in
   *     incr i;
   *     Dumb_progress.update bar !i;
   *     let p = Search_state.program_exn ss v in
   *     Tree_ball.Rename_leaves.enumerate_d3
   *       (module Op)
   *       Cad_gen_pattern.all_renames p
   *       (fun p' ->
   *         let v' = F.eval (Value.eval params) p' in
   *         let cost' = Option.value_exn (Search_state.cost_of ss v) in
   *         let c' = Hashtbl.find_exn classes v' in
   *         Union_find.union c c'));
   * let class_stats = Hashtbl.create (module Int) in
   * Hashtbl.iter classes ~f:(fun c ->
   *     let stats = Hashtbl.find_or_add class_stats (Union_find.get c) ~default:(fun () -> { size = 0 }) in
   *     stats.size <- stats.size + 1);
   * Fmt.epr "States: %d, Classes: %d\n" (Hashtbl.length classes) (Hashtbl.length class_stats) *)

  let local_search_equivalent params =
    let enum = new Parent.synthesizer params in
    let (_ : _) = enum#run in
    let ss = enum#get_search_state in

    let module F = Flat_program.Make (Op) in
    let module G = Graph.Imperative.Graph.Concrete (Value) in
    let module C = Graph.Components.Make (G) in
    let g = G.create () in

    let states = Search_state.states ss in
    Dumb_progress.with_bar (Dumb_progress.basic_bar @@ List.length states) @@ fun bar ->
    List.iteri states ~f:(fun i v ->
        let _cost = Option.value_exn (Search_state.cost_of ss v) in
        Dumb_progress.update bar i;
        let p = Search_state.program_exn ss v in

        let n_ball = ref 0 in
        Tree_ball.Rename_leaves.enumerate_d3
          (module Op)
          Cad_gen_pattern.all_renames p
          (fun p' ->
            incr n_ball;
            let v' = F.eval (Value.eval params) p' in
            let _cost' = Option.value_exn (Search_state.cost_of ss v') in
            (* if cost = cost' then *) G.add_edge g v v'));
    let n_components, comp_of = C.scc g in
    print_s [%message (List.length states : int) (n_components : int)];
    (n_components, comp_of)

  type component_stat = { mutable hit_count : int; mutable keep_count : int }

  class synthesizer params =
    let _bench = Params.get params bench in
    let _dist = Cad_conc.dist params in
    let n_components, component = local_search_equivalent params in
    object (self)
      inherit Parent.synthesizer params as super

      val search_thresh = Params.get params search_thresh

      val diversity = Params.get params diversity

      val mutable retain_thresh = Params.get params retain_thresh

      val mutable retain_power = 1

      val component_table = Array.init n_components ~f:(fun _ -> { hit_count = 0; keep_count = 0 })

      val search_neighbors =
        match Params.get params local_search with
        | `Bounded -> bounded_search params _bench (Params.get params ball_width)
        | `Stochastic -> stochastic_search params _bench
        | `Leaf -> leaf_search params _bench

      method get_stats v = component_table.(component v)

      method print_stats =
        let hit_rate = Array.fold component_table ~f:(fun r s -> r + if s.hit_count > 0 then 1 else 0) ~init:0 in
        let keep_rate = Array.fold component_table ~f:(fun r s -> r + if s.keep_count > 0 then 1 else 0) ~init:0 in
        let states_seen = Array.fold component_table ~f:(fun r s -> r + s.hit_count) ~init:0 in
        Fmt.epr "Hit coverage: %d/%d\nKeep coverage: %d/%d\nStates seen: %d\n" hit_rate (Array.length component_table)
          keep_rate (Array.length component_table) states_seen

      method hit_state v =
        let stats = self#get_stats v in
        stats.hit_count <- stats.hit_count + 1

      method keep_state v =
        let stats = self#get_stats v in
        stats.keep_count <- stats.keep_count + 1

      method distance = _dist

      method dedup_states states =
        states
        |> List.filter ~f:(fun (s, _, _) -> not (Search_state.mem search_state s))
        |> List.dedup_and_sort ~compare:(fun (s, _, _) (s', _, _) -> [%compare: Value.t] s s')

      method find_close_states search_thresh new_states =
        List.filter_map new_states ~f:(fun (v, op, args) ->
            let d = self#distance v _output in
            if Float.(d <= search_thresh) then Some (d, v, op, args) else None)

      method search_close_states new_states =
        let close_states = self#find_close_states search_thresh new_states in
        Fmt.epr "Searching %d/%d neighborhoods\n%!" (List.length close_states) (List.length new_states);

        let closest = ref None in
        let closest_dist = ref Float.infinity in

        List.iter close_states ~f:(fun (d, _, op, args) ->
            let center = Search_state.program_of_op_args_exn search_state op args in
            let center_value = Program.eval (Value.eval params) center in
            let center_dist = self#distance center_value _output in

            if Float.(center_dist < !closest_dist) then (
              closest_dist := center_dist;
              closest := Some (Program.eval (Value.eval params) center));

            search_neighbors
              ~view:(fun v ->
                let d = self#distance _output v in
                if Float.(d < !closest_dist) then (
                  closest_dist := d;
                  closest := Some v))
              self center
            @@ fun p ->
            let final_value_dist = Params.get params final_value_dist
            and final_program_dist = Params.get params final_program_dist in
            final_value_dist := d;
            final_program_dist := Float.of_int @@ Tree_dist.zhang_sasha ~eq:[%compare.equal: Op.t] center p;
            raise @@ Parent.Done p);

        Option.iter !closest
          ~f:(Fmt.epr "Goal:\n%a\nClosest (%f):\n%a\n%!" Cad_conc.pprint _output !closest_dist Cad_conc.pprint)

      method sample_diverse_states new_states =
        let new_states_a = Array.of_list new_states in
        let all_states =
          Array.of_list @@ List.map new_states ~f:(fun (v, _, _) -> v) @ Search_state.states search_state
        in
        let old = List.range (List.length new_states) (Array.length all_states)
        and new_ = List.range 0 (List.length new_states) in
        sample_pairwise self#distance retain_thresh all_states old new_ |> List.map ~f:(fun i -> new_states_a.(i))

      method sample_states cost new_states =
        let to_keep =
          if diversity then self#sample_diverse_states new_states
          else
            let states =
              List.map new_states ~f:(fun (v, op, args) ->
                  let d = self#distance v _output in
                  (d, (v, op, args)))
              |> List.sort ~compare:(fun (d, _) (d', _) -> [%compare: float] d d')
            in
            let n_keep = Int.pow cost retain_power + 5 in
            List.take states n_keep |> List.map ~f:Tuple.T2.get2
        in
        Fmt.epr "Retained %d/%d new states\n%!" (List.length to_keep) (List.length new_states);
        to_keep

      method! generate_states cost =
        let new_states = super#generate_states cost |> self#dedup_states in
        List.iter new_states ~f:(fun (v, _, _) -> self#hit_state v);
        self#print_stats;
        self#search_close_states new_states;
        let sampled_states = self#sample_states cost new_states in
        List.iter new_states ~f:(fun (v, _, _) -> self#keep_state v);
        sampled_states

      method! run =
        let rec reduce_retain_thresh () =
          match super#run with
          | Some p -> Some p
          | None ->
              (* TODO: Should we clear the search space here? *)
              Search_state.clear search_state;
              retain_thresh <- retain_thresh /. 2.0;
              retain_power <- retain_power + 1;
              reduce_retain_thresh ()
        in
        reduce_retain_thresh ()
    end

  let synth params = Option.iter (new synthesizer params)#run ~f:(fun p -> eprint_s [%message (p : Op.t Program.t)])
end

let cli =
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; spec ] in
  let open Command.Let_syntax in
  Command.basic ~summary:(sprintf "Diversity sampling for %s" Lang.name)
  @@ [%map_open
       let params = Dumb_params.Spec.cli spec in
       Synth_utils.run_synth
         (fun params -> new synthesizer params)
         params
         (Option.iter ~f:(fun p -> eprint_s [%message (p : Lang.Op.t Program.t)]))]
