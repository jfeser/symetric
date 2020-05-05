module Fresh = Utils.Fresh
open Utils.Collections
module Gr = Grammar

module Log = Utils.Make_log (struct
  let src = Logs.Src.create "staged-synth.synth"
end)

let prune = ref true

and debug = ref false

let param =
  let open Command in
  let open Command.Let_syntax in
  [%map_open
    let no_prune = flag "no-prune" no_arg ~doc:"prune the search graph"
    and debug_p = flag "debug" no_arg ~doc:"enable debug printing" in
    prune := not no_prune;
    debug := debug_p]

let to_contexts term args =
  let term, holes = Grammar.Term.with_holes term in
  let preorder_names =
    List.map holes ~f:(fun (sym, idx, id) -> (idx, (id, sym)))
    |> Map.of_alist_exn (module Int)
  in
  let ctx =
    List.map args ~f:(fun (idx, sym, value) ->
        let id, sym' = Map.find_exn preorder_names idx in
        if String.(sym' <> sym) then
          failwith
          @@ sprintf "In term %s expected a %s at index %d but got %s"
               (Gr.Term.to_string term) sym' idx sym;
        (id, value))
    |> Map.of_alist_exn (module String)
  in
  (term, ctx)

module Make
    (Sketch : Sigs.SKETCH)
    (S : Sigs.CODE)
    (L : Sigs.LANG with type 'a code = 'a S.t)
    (C : Sigs.CACHE with type value = L.Value.t and type 'a code = 'a S.t) =
struct
  module V = struct
    module T = struct
      type state = { cost : int; symbol : string }
      [@@deriving compare, hash, sexp]

      type code = {
        cost : int;
        rule :
          ((L.Value.t, bool S.t) Semantics.t
          [@compare.ignore] [@sexp.opaque])
          Gr.Rule.t;
      }
      [@@deriving compare, hash, sexp]

      type arg = { id : int; n_args : int } [@@deriving compare, hash, sexp]

      type t = State of state | Code of code | Arg of arg
      [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)

    let pp fmt v = Sexp.pp_hum fmt @@ [%sexp_of: t] v

    let to_state = function State s -> s | _ -> failwith "Not a state"

    let to_code = function Code s -> s | _ -> failwith "Not a code"

    let equal = [%compare.equal: t]
  end

  module E = struct
    type t = int [@@deriving compare]

    let default = -1
  end

  module G = struct
    module X = struct
      let graph_attributes _ = []

      let default_vertex_attributes _ = []

      let vertex_name v = [%sexp_of: V.t] v |> Sexp.to_string |> sprintf "%S"

      let vertex_label = function
        | V.State v -> sprintf "%s : %d" v.symbol v.cost
        | Code v -> Gr.Term.to_string @@ Gr.Rule.rhs v.rule
        | Arg x -> sprintf "%d" x.n_args

      let vertex_attributes v =
        let shape = match v with V.Arg _ -> [ `Shape `Circle ] | _ -> [] in
        let label = [ `Label (vertex_label v) ] in
        shape @ label

      let get_subgraph _ = None

      let default_edge_attributes _ = []

      let edge_attributes (_, idx, _) =
        if idx >= 0 then [ `Label (sprintf "%d" idx) ] else []

      include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (V) (E)
    end

    include X
    include Graph.Graphviz.Dot (X)
    include Graph.Traverse.Dfs (X)
    include Graph.Oper.P (X)

    module Reachability =
      Graph.Fixpoint.Make
        (X)
        (struct
          type vertex = E.vertex

          type edge = E.t

          type g = t

          type data = bool

          let direction = Graph.Fixpoint.Forward

          let equal = Bool.( = )

          let join = ( || )

          let analyze _ x = x
        end)

    module Reverse_reachability =
      Graph.Fixpoint.Make
        (X)
        (struct
          type vertex = E.vertex

          type edge = E.t

          type g = t

          type data = bool

          let direction = Graph.Fixpoint.Backward

          let equal = Bool.( = )

          let join = ( || )

          let analyze _ x = x
        end)

    module Topo = Graph.Topological.Make_stable (X)

    let filter_vertex g ~f =
      fold_vertex (fun v g -> if f v then remove_vertex g v else g) g g

    let add_edges g vs =
      List.fold_left ~init:g ~f:(fun g (v, v') -> add_edge g v v') vs

    let reverse g =
      fold_edges (fun v v' g -> add_edge (remove_edge g v v') v' v) g g
  end

  let cache = C.empty ()

  let bind_io, iter_inputs, output =
    let io =
      Nonlocal_let.let_ S.let_global @@ fun () ->
      S.Sexp.input () |> S.Sexp.to_list
    in
    let background =
      List.mapi Sketch.background ~f:(fun idx sym ->
          ( sym,
            Nonlocal_let.let_ S.let_global @@ fun () ->
            S.let_
              (S.Sexp.List.get (io.value ()) (S.Int.int idx) |> S.Sexp.to_list)
            @@ fun value_sexps ->
            S.let_ (S.Sexp.List.length value_sexps) @@ fun len ->
            S.Array.init len (fun vidx ->
                S.Sexp.List.get value_sexps vidx
                |> L.Value.of_sexp sym |> L.Value.code_of) ))
    in
    let output =
      Nonlocal_let.let_ S.let_global @@ fun () ->
      S.Sexp.List.get (io.value ()) (S.Int.int (List.length Sketch.background))
      |> L.Value.of_sexp Sketch.output
      |> L.Value.code_of
    in
    let bind f =
      io.bind @@ fun () ->
      output.bind @@ fun () ->
      let rec bind_all = function
        | [ x ] -> x.Nonlocal_let.bind f
        | x :: xs -> x.bind @@ fun () -> bind_all xs
        | [] -> assert false
      in
      bind_all (List.map ~f:(fun (_, v) -> v) background)
    and iter_inputs sym f =
      List.filter_map background ~f:(fun (sym', inputs) ->
          if String.(sym = sym') then Some (inputs.value ()) else None)
      |> List.map ~f:(fun inputs ->
             S.Array.iter inputs ~f:(fun input -> f @@ L.Value.of_code input))
      |> S.sseq
    and output () = L.Value.of_code @@ output.value () in
    (bind, iter_inputs, output)

  let cache_iter ~sym ~size ~f cache =
    let open S in
    let open S.Int in
    match size with
    | 0 -> iter_inputs sym f
    | size -> C.iter ~sym ~size:(int size) ~f cache

  let debug_print msg = if !debug then S.eprint (msg ^ "\n") else S.unit

  module Reconstruct = struct
    type ctx = { graph : G.t; cache : C.cache C.code }

    let rec print_term print_nt = function
      | Gr.Nonterm nt -> print_nt nt
      | App (func, args) ->
          S.print (sprintf "(App %s (" func)
          :: List.concat_map args ~f:(fun arg ->
                 print_term print_nt arg @ [ S.print " " ])
          @ [ S.print "))\n" ]
      | As (t, _) -> print_term print_nt t

    let rec of_args ({ cache; _ } as ctx) target term args k =
      let cache = C.of_code cache in
      let pterm = Gr.to_preorder (term : _ Gr.Term.t :> Gr.Untyped_term.t) in
      let check_one args =
        let term, eval_ctx =
          let eval_args =
            List.map args ~f:(fun (state, idx, v) -> (idx, state.V.symbol, v))
          in
          to_contexts term eval_args
        in
        let found =
          match
            L.Value.(
              L.eval eval_ctx (term : _ Gr.Term.t :> Gr.Untyped_term.t)
              = of_code target)
          with
          | `Static b -> S.Bool.bool b
          | `Dyn b -> S.Bool.(b)
        in
        S.ite found
          (fun () ->
            S.sseq
              [
                S.sseq
                @@ print_term
                     (fun (_, idx) ->
                       let state, _, value =
                         List.find_exn args ~f:(fun (_, idx', _) -> idx = idx')
                       in
                       [ reconstruct ctx value state ])
                     pterm;
                k ();
              ])
          (fun () -> S.unit)
      in
      let check_all =
        List.fold_left args ~init:check_one ~f:(fun check (arg_idx, arg_node) ->
            let sym = arg_node.V.symbol and size = arg_node.V.cost in
            fun ctx ->
              cache_iter ~sym ~size
                ~f:(fun value -> check ((arg_node, arg_idx, value) :: ctx))
                cache)
      in
      check_all []

    (** Reconstruct a target assuming that it was produced by a particular code
      node. *)
    and of_code ({ graph = g; _ } as ctx) target code k =
      let term = Gr.Rule.rhs @@ code.V.rule and args = G.succ g (V.Code code) in
      if List.is_empty args then of_args ctx target term [] k
      else
        List.map args ~f:(fun n ->
            of_args ctx target term
              (G.succ_e g n |> List.map ~f:(fun (_, i, v) -> (i, V.to_state v)))
              k)
        |> S.sseq

    and of_state ({ graph = g; _ } as ctx) state target k =
      match G.succ g (State state) with
      | [] when String.(state.V.symbol <> "V") ->
          S.sseq
            [
              S.print @@ sprintf "(Nonterm %s " state.symbol;
              L.Value.of_code target |> L.Value.sexp_of |> S.Sexp.print;
              S.print ")";
              k ();
            ]
      | [] -> S.sseq [ S.print "Input"; k () ]
      | deps ->
          List.map deps ~f:(fun n -> of_code ctx target (V.to_code n) k)
          |> S.sseq

    and reconstruct { graph = g; cache } target state =
      let open S in
      let func =
        let func_t =
          let args_t =
            Tuple.mk_type (type_of cache) (type_of (L.Value.code_of target))
          in
          Func.mk_type args_t unit_t
        and func_name = sprintf "reconstruct_%s_%d" state.V.symbol state.cost in

        Func.func func_name func_t (fun args ->
            let_ (Tuple.fst args) @@ fun cache ->
            let_ (Tuple.snd args) @@ fun target ->
            Log.debug (fun m -> m "Building %s." func_name);
            S.sseq
              [
                S.print "(Choose (";
                ( of_state { graph = g; cache } state target @@ fun () ->
                  S.sseq [ S.print "))\n"; S.return ] );
              ])
      in
      Func.apply func (Tuple.create cache (L.Value.code_of target))
  end

  let contract_state g =
    G.fold_vertex
      (fun v g ->
        match v with
        | State _ ->
            (* Add edges from this node to its grandchildren. *)
            G.succ g v
            |> List.concat_map ~f:(G.succ g)
            |> List.map ~f:(fun v' -> (v, v'))
            |> G.add_edges g
        | _ -> g)
      g g
    |> G.filter_vertex ~f:(fun v -> match v with State _ -> false | _ -> true)

  let find_binding t n =
    let t = Gr.to_preorder t in
    Gr.find_binding n t

  let binding_deps t n =
    match find_binding t n with
    | Some t' -> Gr.non_terminals t' |> List.map ~f:(fun (_, i) -> i)
    | None -> failwith (sprintf "Could not find binding: %s" n)

  let binding_ctx term binds args =
    let term, ctx = to_contexts term args in
    Gr.Term.bindings term
    |> List.filter ~f:(fun (b, _) ->
           List.mem binds ~equal:[%compare.equal: Gr.Bind.t] b)
    |> List.map ~f:(fun (bind, term') ->
           (bind, L.eval ctx (term' : _ Gr.Term.t :> Gr.Untyped_term.t)))
    |> Map.of_alist_exn (module Gr.Bind)

  let argument_graph rule =
    let module G = Graph.Persistent.Digraph.ConcreteBidirectional (Int) in
    let module Topo = Graph.Topological.Make_stable (G) in
    let indexes_g =
      List.init ~f:Fun.id (Gr.Term.n_holes @@ Gr.Rule.rhs rule)
      |> List.fold_left ~init:G.empty ~f:G.add_vertex
    in
    let g =
      List.fold_left (Gr.Rule.semantics rule) ~init:indexes_g ~f:(fun g ->
        function
        | Semantics.Prop { deps; out } ->
            List.concat_map deps
              ~f:(binding_deps @@ (Gr.Rule.rhs rule :> Gr.Untyped_term.t))
            |> List.fold_left ~init:g ~f:(fun g node -> G.add_edge g node out)
        | _ -> g)
    in
    Topo.fold (fun v l -> v :: l) g [] |> List.rev

  let search_graph ?(prune = !prune) max_cost =
    (* Compute search graph. *)
    let fresh = Fresh.create () and g = ref G.empty in

    let module Key = struct
      type t = (int * V.state) list [@@deriving compare, hash, sexp]
    end in
    let arg_multiedges = Hashtbl.create (module Key) in
    let norm k =
      List.sort ~compare:(fun (i, _) (i', _) -> [%compare: int] i i') k
    in
    let find_arg k = Hashtbl.find arg_multiedges @@ norm k
    and add_arg k v = Hashtbl.set arg_multiedges ~key:(norm k) ~data:v in

    let sources =
      List.map Sketch.background ~f:(fun symbol -> V.State { symbol; cost = 0 })
      |> Set.of_list (module V)
    in

    (* Add initial states. *)
    g := Set.fold sources ~init:!g ~f:G.add_vertex;

    for cost = 0 to max_cost do
      (* Select the rules that have cheap enough right hand sides. *)
      let rules =
        List.filter L.grammar ~f:(fun rule ->
            let rhs = Gr.Rule.rhs rule in
            let size = Gr.Term.size rhs and n_holes = Gr.Term.n_holes rhs in
            size - n_holes <= cost)
      in

      List.iter rules ~f:(fun rule ->
          Log.debug (fun m -> m "Adding %a at cost %d." Gr.Rule.pp rule cost);
          (* Add state and code nodes for the rule's output. *)
          let rhs_v = V.Code { cost; rule } in

          (* Add an arg node for each partition permutation of the argument
             costs. *)
          let term = Gr.Rule.rhs rule in
          let non_terminals =
            Gr.to_preorder (term :> Gr.Untyped_term.t)
            |> Gr.non_terminals
            |> List.sort ~compare:(fun (_, i) (_, i') -> [%compare: int] i i')
            |> List.map ~f:(fun (x, _) -> x)
            |> Array.of_list
          in
          let n_holes = Array.length non_terminals
          and size = Gr.Term.size term in
          let open Combinat in
          let parts =
            Partition.With_zeros.(
              to_list @@ create ~n:(cost - (size - n_holes)) ~parts:n_holes)
          in
          Log.debug (fun m -> m "Parts %a" Fmt.(Dump.(list @@ list int)) parts);
          let permuted =
            let filter costs =
              List.for_alli costs ~f:(fun i cost ->
                  G.mem_vertex !g (V.State { symbol = non_terminals.(i); cost }))
            in
            List.concat_map parts ~f:(fun parts ->
                Permutation.Restricted.(
                  Of_list.(create parts filter |> to_list)))
          in
          (* Log.debug (fun m ->
           *     m "Permuted %a" Fmt.(Dump.(list @@ list int)) permuted); *)
          let deduped =
            List.dedup_and_sort ~compare:[%compare: int list] permuted
          in
          List.iter deduped ~f:(fun costs ->
              (* Log.debug (fun m -> m "Inserting %a" Fmt.(Dump.(list int)) costs); *)
              let costs = Array.of_list costs in
              assert (Array.length costs = n_holes);

              let deps =
                Gr.to_preorder (term :> Gr.Untyped_term.t)
                |> Gr.non_terminals
                |> List.map ~f:(fun (symbol, arg_idx) ->
                       (arg_idx, { V.cost = costs.(arg_idx); symbol }))
              in
              match find_arg deps with
              | Some arg -> g := G.add_edge !g rhs_v arg
              | None ->
                  let all_deps_exist =
                    List.for_all deps ~f:(fun (_, v) ->
                        G.mem_vertex !g (V.State v))
                  in
                  if all_deps_exist then (
                    let arg =
                      V.Arg { id = Fresh.int fresh; n_args = n_holes }
                    in
                    g := G.add_edge !g rhs_v arg;
                    List.iter deps ~f:(fun (arg_idx, state) ->
                        let edge = (arg, arg_idx, V.State state) in
                        g := G.add_edge_e !g edge);
                    add_arg deps arg ));

          if G.mem_vertex !g rhs_v then
            let lhs_v = V.State { cost; symbol = Gr.Rule.lhs rule } in
            g := G.add_edge !g lhs_v rhs_v)
    done;
    let g = !g in

    (* Prune nodes that the sinks don't depend on and nodes that can't reach a
       source. *)
    let rec prune_loop g =
      let is_source = Set.mem sources
      and is_sink = function
        | V.State { symbol } -> String.(symbol = Sketch.output)
        | _ -> false
      in
      let sink_reachable = G.Reachability.analyze is_sink g
      and source_reachable = G.Reverse_reachability.analyze is_source g in
      let g' =
        G.fold_vertex
          (fun v g ->
            let should_keep =
              sink_reachable v && source_reachable v
              && match v with Arg x -> x.n_args = G.out_degree g v | _ -> true
            in
            if should_keep then g
            else
              (* Log.debug (fun m -> m "Pruning vertex %a" V.pp v); *)
              G.remove_vertex g v)
          g g
      in
      if G.nb_vertex g > G.nb_vertex g' || G.nb_edges g > G.nb_edges g' then
        prune_loop g'
      else g'
    in

    if prune then prune_loop g else g

  let vlet v f = S.let_ (L.Value.code_of v) (fun v' -> f (L.Value.of_code v'))

  let reconstruct g state value =
    (* Don't try to reconstruct a value that isn't of the same kind as the
       expected output. *)
    if String.(Sketch.output <> state.V.symbol) then S.unit
    else
      let reconstruct_code =
        S.sseq
          [
            debug_print "Starting reconstruction";
            Reconstruct.reconstruct
              { graph = g; cache = cache.value () |> C.code_of }
              (output ()) state;
            S.exit;
          ]
      in
      match L.Value.(value = output ()) with
      | `Static true -> reconstruct_code
      | `Dyn eq -> S.ite eq (fun () -> reconstruct_code) (fun () -> S.unit)
      | `Static false ->
          Log.debug (fun m -> m "Skipping reconstruction: no equality");
          S.unit

  let insert state term value =
    let sym = state.V.symbol and cost = state.V.cost in
    C.put ~sym ~size:cost (cache.value ()) value

  let deps_satisfied (term : [ `Open | `Closed ] Gr.Term.t) deps bindings =
    let seen = List.map bindings ~f:(fun (i, _, _) -> i) in
    Log.debug (fun m -> m "Seen: %a" Fmt.(Dump.list int) seen);
    let ret =
      List.concat_map deps ~f:(fun n ->
          let args = binding_deps (term :> Gr.Untyped_term.t) n in
          Log.debug (fun m -> m "Deps (%s): %a" n Fmt.(Dump.list int) args);
          args)
      |> List.for_all ~f:(List.mem seen ~equal:[%compare.equal: int])
    in
    Log.debug (fun m -> m "Sat: %b" ret);
    ret

  let fill_with_pred rule k bindings =
    let term = Gr.Rule.rhs rule in
    let preds =
      List.filter_map (Gr.Rule.semantics rule) ~f:(function
        | Semantics.Pred { deps; func } when deps_satisfied term deps bindings
          ->
            Some (deps, func)
        | _ -> None)
    in
    let deps = List.concat_map preds ~f:(fun (deps, _) -> deps)
    and preds = List.map preds ~f:(fun (_, pred) -> pred) in
    match preds with
    | [] -> k bindings
    | ps ->
        Log.debug (fun m -> m "Inserting predicate.");
        let ctx = binding_ctx term deps bindings in
        let pred =
          List.map ps ~f:(fun p -> p ctx) |> List.reduce_exn ~f:S.Bool.( && )
        in
        S.ite pred (fun () -> k bindings) (fun () -> S.unit)

  let fill_with_prop rule arg_n arg k bindings =
    let term = Gr.Rule.rhs rule in
    let prop =
      List.find_map (Gr.Rule.semantics rule) ~f:(function
        | Semantics.Prop { out; deps; func }
          when out = arg_n && deps_satisfied term deps bindings ->
            Some (func, deps)
        | _ -> None)
    in
    match prop with
    | Some (f, deps) ->
        Log.debug (fun m -> m "Inserting propagator.");
        let bindings =
          (arg_n, arg.V.symbol, f (binding_ctx term deps bindings)) :: bindings
        in
        k bindings
    | None ->
        cache_iter ~sym:arg.V.symbol ~size:arg.V.cost (cache.value ())
          ~f:(fun v ->
            let bindings = (arg_n, arg.V.symbol, v) :: bindings in
            k bindings)

  let fill_args g state rule args =
    let term = Gr.Rule.rhs rule in

    let fill_inner bindings =
      let term, ctx = to_contexts term bindings in
      vlet (L.eval ctx (term : _ Gr.Term.t :> Gr.Untyped_term.t))
      @@ fun value ->
      S.seq (reconstruct g state value) (insert state term value)
    in

    let arg_order = argument_graph rule in
    Log.debug (fun m ->
        m "Argument order for %s: %a" (Gr.Term.to_string term)
          (Fmt.Dump.list Fmt.int) arg_order);

    let fill =
      List.fold_left arg_order
        ~init:(fill_with_pred rule @@ fill_inner)
        ~f:(fun fill arg_n ->
          fill_with_pred rule @@ fill_with_prop rule arg_n args.(arg_n) @@ fill)
    in
    fill []

  (** Generate code to fill in a state set from a single grammar rule. *)
  let fill_code g state code =
    G.succ g (V.Code code)
    |> List.map ~f:(fun arg_node ->
           let args =
             G.succ_e g arg_node
             |> List.map ~f:(fun (_, idx, node) -> (idx, V.to_state node))
             |> List.sort ~compare:(fun (i, _) (i', _) -> [%compare: int] i i')
             |> List.map ~f:(fun (_, state) -> state)
             |> List.to_array
           in
           fill_args g state code.V.rule args)

  let fill_state g state =
    Log.debug (fun m -> m "Generating code for %s:%d" state.V.symbol state.cost);
    S.sseq
      [
        debug_print (sprintf "Filling %s at cost %d" state.V.symbol state.cost);
        G.succ g (V.State state)
        |> List.concat_map ~f:(fun code_node ->
               fill_code g state @@ V.to_code code_node)
        |> S.sseq;
      ]

  let enumerate max_cost =
    let g = search_graph max_cost in

    (* Generate a graph that only contains the state nodes. *)
    let lhs_g = contract_state g |> G.reverse in

    let open S in
    cache.bind @@ fun () ->
    bind_io @@ fun () ->
    (* Fold over the state nodes, generating code to fill them. *)
    G.Topo.fold
      (fun v code ->
        let state = V.to_state v in
        code @ [ fill_state g state ])
      lhs_g []
    |> sseq
end
