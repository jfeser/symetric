open Core_profiler.Std_offline

module Probes_intf = struct
  module type S = sig
    type search_state

    val fill : (search_state -> int -> unit) option
  end
end

module Make
    (Lang : Lang_intf.S)
    (Search_state : Search_state_intf.S
                      with type op = Lang.Op.t
                       and type abs = Lang.Abs.t
                       and type params = Lang.params
                       and type type_ = Lang.Type.t)
    (Refine : Refine_intf.S
                with type op := Lang.Op.t
                 and module Search_state := Search_state
                 and module Abs := Lang.Abs)
    (Probes : Probes_intf.S with type search_state := Search_state.t) =
struct
  open Lang

  open Cone.Make (Search_state.G)

  open Search_state

  (* Profiling probes *)
  let state_nodes = Probe.create ~name:"state_nodes" ~units:Profiler_units.Int

  let arg_nodes = Probe.create ~name:"arg_nodes" ~units:Profiler_units.Int

  let states = Probe.create ~name:"states" ~units:Profiler_units.Int

  let refinements = Probe.create ~name:"refinements" ~units:Profiler_units.Int

  let fill_probe = Option.value Probes.fill ~default:(fun _ _ -> ())

  let fold_range ~init ~f lo hi =
    let rec fold_range acc i =
      if i >= hi then acc
      else
        let acc' = f acc i in
        fold_range acc' (i + 1)
    in
    fold_range init lo

  let roots ss =
    let is_subset v ~of_:v' =
      State.cost ss v = State.cost ss v'
      && Abs.leq (State.state ss v) (State.state ss v')
    in
    G.Fold.V.filter_map (graph ss)
      ~f:(Node.match_ ~state:Option.return ~args:(fun _ -> None))
    |> List.fold_left ~init:[] ~f:(fun roots v ->
           match List.find roots ~f:(fun v' -> is_subset v ~of_:v') with
           | Some _ -> roots
           | None ->
               v :: List.filter roots ~f:(fun v' -> not (is_subset v' ~of_:v)))

  module State_set = struct
    type t = type_:Type.t -> cost:int -> State.t list
  end

  let state_set_full ss =
    let cost_tbl = Hashtbl.create (module Int) in
    fun ~type_ ~cost ->
      let type_tbl =
        match Hashtbl.find cost_tbl cost with
        | Some type_tbl -> type_tbl
        | None ->
            let states =
              states_of_cost ss cost
              |> List.filter ~f:(fun s ->
                     let args = G.succ (graph ss) (Node.of_state s) in
                     List.is_empty args
                     || List.exists args ~f:(fun v ->
                            match Node.to_args_exn v |> Args.label ss with
                            | Merge -> true
                            | _ -> false))
            in
            let type_tbl = Hashtbl.create (module Type) in
            List.iter states ~f:(fun s ->
                Hashtbl.add_multi type_tbl ~key:(State.type_ ss s) ~data:s);
            type_tbl
      in
      Hashtbl.find type_tbl type_ |> Option.value ~default:[]

  let state_set_roots ss : State_set.t =
    let module Key = struct
      type t = int * Type.t [@@deriving compare, hash, sexp]
    end in
    let tbl = Hashtbl.create (module Key) in
    let root_states = roots ss in
    List.iter root_states ~f:(fun s ->
        Hashtbl.add_multi tbl ~key:(State.cost ss s, State.type_ ss s) ~data:s);
    fun ~type_ ~cost ->
      Hashtbl.find tbl (cost, type_) |> Option.value ~default:[]

  let fill_cost ss (state_set : State_set.t) ops cost =
    let size = G.nb_vertex @@ graph ss in
    if cost = 1 then
      (* Add inputs to the state space graph. *)
      List.iter ops ~f:(fun op ->
          if Op.arity op = 0 then
            insert_hyper_edge ss [] op 1 ~state:(Abs.eval (params ss) op []))
    else if cost > 1 then (
      let arg_cost = cost - 1 in

      List.iter ops ~f:(fun op ->
          let arity = Op.arity op in
          let arg_types = Op.args_type op |> Array.of_list in
          let module Comp = Combinat.Composition in
          if arity > 0 then
            Comp.create ~n:arg_cost ~k:arity
            |> Comp.iter ~f:(fun arg_costs ->
                   let add_hyper_edges =
                     fold_range
                       ~init:(fun args -> insert_hyper_edge ss args op cost)
                       ~f:(fun f i args ->
                         state_set
                           ~cost:(Combinat.Int_array.get arg_costs i)
                           ~type_:arg_types.(i)
                         |> List.iter ~f:(fun v -> f (v :: args)))
                       0 arity
                   in
                   add_hyper_edges []));

      let size' = G.nb_vertex @@ graph ss in

      let state_vs = G.Fold.V.filter (graph ss) ~f:Node.is_state in
      Probe.record state_nodes @@ List.length state_vs;
      Probe.record arg_nodes
      @@ (G.Fold.V.filter (graph ss) ~f:Node.is_args |> List.length);
      Probe.record arg_nodes
      @@ (G.Fold.V.filter (graph ss) ~f:Node.is_args |> List.length);
      Probe.record states
      @@ ( List.map state_vs ~f:(fun v -> Node.to_state_exn v |> State.state ss)
         |> List.dedup_and_sort ~compare:[%compare: Abs.t]
         |> List.length );
      fill_probe ss cost;

      Fmt.epr "Filling (cost=%d): size before=%d, after=%d, added %f%%\n%!" cost
        size size'
        Float.(-(100.0 - (of_int size' / of_int size * 100.0))) )

  let state_set ss =
    match (params ss).state_set with
    | `Full -> state_set_full ss
    | `Roots -> state_set_roots ss

  let[@landmark "fill"] fill_up_to_cost ss ops cost =
    let rec fill c =
      if c > cost then ()
      else
        let states = state_set ss in
        fill_cost ss states ops c;
        fill (c + 1)
    in
    fill 1

  exception Found_solution of Op.t Program.t

  let with_size ss f =
    let vsize = G.Fold.V.filter (graph ss) ~f:Node.is_state |> List.length in
    let ret = f ss in
    let vsize' = G.Fold.V.filter (graph ss) ~f:Node.is_state |> List.length in

    Fmt.epr "Pruning: #states before=%d, after=%d, removed %f%%\n%!" vsize
      vsize'
      Float.(100.0 - (of_int vsize' / of_int vsize * 100.0));

    ret

  let apply_refinement ss (refinement : Refine.Refinement.t) =
    [%test_pred: Refine.Refinement.t] ~message:"empty refinement"
      (fun r -> not @@ Map.is_empty r)
      refinement;
    [%test_pred: Refine.Refinement.t] ~message:"no-op refinement"
      (Map.for_all ~f:(fun Refine.Refinement.{ old; new_ } ->
           not @@ Set.equal old new_))
      refinement;

    let new_states =
      Map.to_alist refinement
      |> List.concat_map ~f:(fun (arg_v, { old; new_ }) ->
             Probe.record refinements 0;

             let cost, type_ =
               let old_state =
                 G.pred (graph ss) @@ Node.of_args arg_v
                 |> List.hd_exn |> Node.to_state_exn
               in
               (State.cost ss old_state, State.type_ ss old_state)
             in

             (* Remove edges to old states *)
             G.pred_e (graph ss) @@ Node.of_args arg_v
             |> List.filter ~f:(fun (state_v, _, _) ->
                    Set.mem old (Node.to_state_exn state_v |> State.state ss))
             |> List.iter ~f:(G.remove_edge_e @@ graph ss);

             (* Insert new states and add edges to args nodes. *)
             let new_states =
               Set.to_list new_
               |> List.map ~f:(fun state ->
                      let (Fresh v' | Stale v') =
                        State.create ss state cost type_
                      in
                      G.add_edge_e (graph ss)
                        (Node.of_state v', -1, Node.of_args arg_v);
                      v')
             in

             new_states)
    in

    fix_up ss;
    new_states

  let apply_summary ss summary =
    List.iter summary ~f:(fun (abs, states) -> insert_merge ss states abs)

  let refute ss target =
    if List.is_empty target then ()
    else
      match Refine.refine ss target with
      | First r ->
          let pre_refine =
            cone (graph ss) @@ List.map ~f:Node.of_state target
          in
          dump_detailed_graph ~suffix:"prerefine" ss pre_refine;

          let post_refine =
            let new_states = with_size ss @@ fun ss -> apply_refinement ss r in
            cone (graph ss) @@ List.map ~f:Node.of_state new_states
          in
          dump_detailed_graph ~suffix:"postrefine" ss post_refine
      | Second p ->
          Fmt.epr "Could not refute: %a" Sexp.pp_hum
            ([%sexp_of: Op.t Program.t] p);
          raise @@ Found_solution p

  let synth params =
    let ss = Search_state.create params
    and ops = Bench.ops params.bench
    and output = Bench.output params.bench in

    let rec fill cost =
      if cost > params.max_cost then ()
      else (
        dump_detailed ~suffix:"before-fill" ss;
        fill_up_to_cost ss ops cost;
        dump_detailed ~suffix:"after-fill" ss;
        Option.iter Refine.summarize ~f:(fun summarize ->
            let input_states = states_of_cost ss cost in
            let summary = summarize ss input_states in
            apply_summary ss summary;
            print_s
              [%message
                "after summarization"
                  (cost : int)
                  (List.length input_states : int)
                  (List.length summary : int)]);
        dump_detailed ~suffix:"after-summarize" ss;
        let targets =
          G.Fold.V.filter_map (graph ss) ~f:(fun v ->
              Node.match_ v
                ~args:(fun _ -> None)
                ~state:(fun v ->
                  if Abs.contains (State.state ss v) output then Some v
                  else None))
        in
        if List.is_empty targets then fill (cost + 1) else refute ss targets;
        fill cost )
    in

    let prog =
      try
        fill 0;
        None
      with Found_solution p -> Some p
    in
    (prog, ss)
end
