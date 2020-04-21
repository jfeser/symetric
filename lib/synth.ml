open! Core
module Fresh = Utils.Fresh
open Utils.Collections
module Gr = Grammar

module Log = Utils.Make_log (struct
  let src = Logs.Src.create "staged-synth.synth"
end)

let () = Log.set_level None

let for_all a f =
  let rec loop i =
    if i >= Bigarray.Array1.dim a then true else f a.{i} && loop (i + 1)
  in
  loop 0

let to_list a = List.init (Bigarray.Array1.dim a) ~f:(fun i -> a.{i})

let rec n_cartesian_product = function
  | [] -> [ [] ]
  | h :: t ->
      let rest = n_cartesian_product t in
      List.concat (List.map ~f:(fun i -> List.map ~f:(fun r -> i :: r) rest) h)

module Arg = struct
  type 'a t = { symbol : string; index : int; value : 'a }
end

let to_contexts term args =
  let term, holes = Grammar.with_holes term in
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
      open V

      let graph_attributes _ = []

      let default_vertex_attributes _ = []

      let vertex_name v = [%sexp_of: V.t] v |> Sexp.to_string |> sprintf "%S"

      let vertex_label = function
        | State v -> sprintf "%s : %d" v.symbol v.cost
        | Code v -> Gr.Term.to_string @@ Gr.Rule.rhs v.rule
        | Arg x -> sprintf "%d" x.n_args

      let vertex_attributes v =
        let shape = match v with Arg _ -> [ `Shape `Circle ] | _ -> [] in
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
    module Topo = Graph.Topological.Make_stable (X)

    let filter_vertex g ~f =
      fold_vertex (fun v g -> if f v then remove_vertex g v else g) g g

    let add_edges g vs =
      List.fold_left ~init:g ~f:(fun g (v, v') -> add_edge g v v') vs

    let reverse g =
      fold_edges (fun v v' g -> add_edge (remove_edge g v v') v' v) g g
  end

  open S.Int
  open C

  let cache = C.empty ()

  let bind_io, inputs, output =
    let io =
      Nonlocal_let.let_ S.let_global @@ fun () ->
      S.Sexp.input () |> S.Sexp.to_list
    in
    let inputs =
      List.mapi Sketch.inputs ~f:(fun idx sym ->
          ( sym,
            Nonlocal_let.let_ S.let_global @@ fun () ->
            S.Sexp.List.get (io.value ()) (S.Int.int idx)
            |> L.Value.of_sexp sym |> L.Value.code_of ))
    in
    let output =
      Nonlocal_let.let_ S.let_global @@ fun () ->
      S.Sexp.List.get (io.value ()) (S.Int.int (List.length Sketch.inputs))
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
      bind_all (List.map ~f:(fun (_, v) -> v) inputs)
    and inputs () =
      List.map inputs ~f:(fun (sym, i) -> (sym, L.Value.of_code @@ i.value ()))
    and output () = L.Value.of_code @@ output.value () in
    (bind, inputs, output)

  let cache_iter ~sym ~size ~f cache =
    let open S in
    sseq
      [
        ite
          (size = int 1)
          (fun () ->
            inputs ()
            |> List.filter ~f:(fun (sym', _) -> Core.String.(sym = sym'))
            |> List.map ~f:(fun (_, v) -> f v)
            |> S.sseq)
          (fun () -> unit);
        C.iter ~sym ~size ~f cache;
      ]

  let debug = false

  let debug_print msg = if debug then S.print msg else S.unit

  let rec of_list = function
    | [] -> S.unit
    | [ l ] -> l
    | l :: ls -> S.seq l (of_list ls)

  let rec case pred default = function
    | [] -> default
    | (v, k) :: bs ->
        S.ite (pred v) (fun () -> k) (fun () -> case pred default bs)

  let rec let_many f = function
    | [] -> f []
    | [ x ] -> S.let_ x (fun x -> f [ x ])
    | x :: xs -> S.let_ x (fun x -> let_many (fun xs -> f (x :: xs)) xs)

  let costs_t = S.Array.mk_type S.Int.type_

  module Reconstruct = struct
    type ctx = { graph : G.t; cache : cache code }

    let rec of_args ({ cache; _ } as ctx) target term args =
      let cache = C.of_code cache in
      let check_one args =
        let term, eval_ctx =
          let eval_args =
            List.map args ~f:(fun (state, idx, v) -> (idx, state.V.symbol, v))
          in
          to_contexts term eval_args
        in
        let found =
          match L.Value.(L.eval eval_ctx term = of_code target) with
          | `Static b -> S.Bool.bool b
          | `Dyn b -> S.Bool.(b)
        in
        S.ite found
          (fun () ->
            S.sseq
              ( S.print ([%sexp_of: _ Gr.Term.t] term |> Sexp.to_string_hum)
              :: List.map args ~f:(fun (state, _, value) ->
                     reconstruct ctx value state) ))
          (fun () -> S.unit)
      in
      let check_all =
        List.fold_left args ~init:check_one ~f:(fun check (arg_idx, arg_node) ->
            let sym = arg_node.V.symbol and size = int arg_node.V.cost in
            fun ctx ->
              cache_iter ~sym ~size
                ~f:(fun value -> check ((arg_node, arg_idx, value) :: ctx))
                cache)
      in
      check_all []

    (** Reconstruct a target assuming that it was produced by a particular code
      node. *)
    and of_code ({ graph = g; _ } as ctx) target code =
      let term = Gr.Rule.rhs @@ code.V.rule and args = G.succ g (V.Code code) in
      if List.is_empty args then of_args ctx target term []
      else
        List.map args ~f:(fun n ->
            of_args ctx target term
              (G.succ_e g n |> List.map ~f:(fun (_, i, v) -> (i, V.to_state v))))
        |> S.sseq

    and of_state ({ graph = g; _ } as ctx) state target =
      G.succ g (State state)
      |> List.map ~f:(fun n -> of_code ctx target @@ V.to_code n)
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
            of_state { graph = g; cache } state target)
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

  let binding_ctx term args =
    let term, ctx = to_contexts term args in
    Gr.Term.bindings term
    |> List.map ~f:(fun (bind, term') -> (bind, L.eval ctx term'))
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

  let search_graph ?(prune = true) max_cost =
    (* Compute search graph. *)
    let fresh = Fresh.create () and g = ref G.empty in

    for cost = 0 to max_cost do
      (* Select the rules that have cheap enough right hand sides. *)
      let rules =
        List.filter L.grammar ~f:(fun rule ->
            Int.(Gr.(Term.size (Rule.rhs rule) <= cost)))
      in

      List.iter rules ~f:(fun rule ->
          (* Add state and code nodes for the rule's output. *)
          let lhs = Gr.Rule.lhs rule and rhs = Gr.Rule.rhs rule in
          let lhs_v = V.State { cost; symbol = lhs }
          and rhs_v = V.Code { cost; rule } in
          g := G.add_edge !g lhs_v rhs_v;

          (* Add an arg node for each partition permutation of the argument
             costs. *)
          let n_holes = Gr.Term.n_holes rhs and size = Gr.Term.size rhs in
          let open Combinat in
          Partition.(
            to_list @@ Int.(create ~n:(cost - size + n_holes) ~parts:n_holes))
          |> List.map ~f:(fun part ->
                 List.init (Bigarray.Array1.dim part) ~f:(fun i -> part.{i}))
          |> List.concat_map ~f:(fun parts ->
                 Permutation.(Of_list.(create parts |> to_list)))
          |> List.dedup_and_sort ~compare:[%compare: int list]
          |> List.iter ~f:(fun costs ->
                 let costs = Array.of_list costs in
                 assert (Int.(Array.length costs = n_holes));
                 let arg = V.Arg { id = Fresh.int fresh; n_args = n_holes } in
                 Gr.to_preorder (rhs :> Gr.Untyped_term.t)
                 |> Gr.non_terminals
                 |> List.iter ~f:(fun (symbol, arg_idx) ->
                        let cost = costs.(arg_idx) in
                        g := G.add_edge !g rhs_v arg;
                        let edge = (arg, arg_idx, V.State { cost; symbol }) in
                        g := G.add_edge_e !g edge)))
    done;
    let g = !g in

    let vertices = G.fold_vertex (fun v vs -> v :: vs) g [] in
    let sinks =
      List.filter vertices ~f:(function
        | State { symbol; _ } -> String.(symbol = Sketch.output)
        | _ -> false)
    and sources =
      List.filter vertices ~f:(function
        | State { symbol; cost = 1 } ->
            List.mem Sketch.inputs symbol ~equal:[%compare.equal: string]
        | _ -> false)
    in

    (* Prune nodes that the sinks don't depend on and nodes that can't reach a
       source. *)
    let should_keep g g_trans v =
      let open V in
      ( List.mem sinks v ~equal
      || List.exists sinks ~f:(fun v' -> G.mem_edge g_trans v' v) )
      && ( List.mem sources v ~equal
         || List.exists sources ~f:(fun v' -> G.mem_edge g_trans v v') )
      && match v with Arg x -> Int.(x.n_args = G.out_degree g v) | _ -> true
    in

    let rec prune_loop g =
      let g_trans = G.transitive_closure g in
      let g' =
        G.fold_vertex
          (fun v g ->
            if should_keep g g_trans v then g else G.remove_vertex g v)
          g g
      in
      if Int.(G.nb_vertex g > G.nb_vertex g' || G.nb_edges g > G.nb_edges g')
      then prune_loop g'
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
    S.sseq
      [
        debug_print
          (sprintf "Inserting (%s -> %s) cost %d" sym (Gr.Term.to_string term)
             cost);
        put ~sym ~size:cost (cache.value ()) value;
      ]

  let fill_args g state rule args =
    let term = Gr.Rule.rhs rule in

    let fill args =
      let term, ctx = to_contexts term args in
      vlet (L.eval ctx term) @@ fun value ->
      S.seq (reconstruct g state value) (insert state term value)
    in

    let arg_order = argument_graph rule in
    Log.debug (fun m ->
        m "Argument order for %s: %a" (Gr.Term.to_string term)
          (Fmt.Dump.list Fmt.int) arg_order);
    let deps_satisfied idx deps =
      let seen = List.take arg_order idx in
      List.concat_map deps ~f:(binding_deps (term :> Gr.Untyped_term.t))
      |> List.for_all ~f:(List.mem seen ~equal:[%compare.equal: int])
    in
    let fill =
      List.foldi arg_order ~init:fill ~f:(fun idx fill arg_n ->
          let arg = args.(arg_n) in

          let preds =
            List.filter_map (Gr.Rule.semantics rule) ~f:(function
              | Pred { deps; func } when deps_satisfied idx deps -> Some func
              | _ -> None)
          in
          let prop =
            List.find_map (Gr.Rule.semantics rule) ~f:(function
              | Prop { out; deps; func }
                when Core.Int.(out = idx) && deps_satisfied idx deps ->
                  Some func
              | _ -> None)
          in

          let fill_prop =
            match prop with
            | Some f ->
                fun args ->
                  fill ((arg_n, arg.V.symbol, f (binding_ctx term args)) :: args)
            | None ->
                fun args ->
                  cache_iter ~sym:arg.V.symbol ~size:(int arg.V.cost)
                    (cache.value ()) ~f:(fun v ->
                      fill ((arg_n, arg.V.symbol, v) :: args))
          in
          let fill_pred =
            match preds with
            | [] -> fill_prop
            | ps ->
                fun args ->
                  let ctx = binding_ctx term args in
                  let pred =
                    List.map ps ~f:(fun p -> p ctx)
                    |> List.fold_left ~init:(S.Bool.bool false) ~f:S.Bool.( && )
                  in
                  S.ite pred (fun () -> fill args) (fun () -> S.unit)
          in
          fill_pred)
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
    G.succ g (V.State state)
    |> List.concat_map ~f:(fun code_node ->
           fill_code g state @@ V.to_code code_node)
    |> S.sseq

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
