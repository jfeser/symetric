open! Core
module Fresh = Utils.Fresh
open Utils.Collections

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

module V = struct
  type state = { cost : int; symbol : string } [@@deriving compare, hash, sexp]

  type code = { cost : int; term : Grammar.Term.t }
  [@@deriving compare, hash, sexp]

  type arg = { id : int; n_args : int } [@@deriving compare, hash, sexp]

  type t = State of state | Code of code | Arg of arg
  [@@deriving compare, hash, sexp]

  let to_state = function State s -> s | _ -> failwith "Not a state"

  let to_code = function Code s -> s | _ -> failwith "Not a code"

  let equal = [%compare.equal: t]
end

module G = struct
  module X = struct
    open V

    let graph_attributes _ = []

    let default_vertex_attributes _ = []

    let vertex_name v = [%sexp_of: V.t] v |> Sexp.to_string |> sprintf "%S"

    let vertex_label = function
      | State v -> sprintf "%s : %d" v.symbol v.cost
      | Code v -> Grammar.Term.to_string v.term
      | Arg x -> sprintf "%d" x.n_args

    let vertex_attributes v =
      let shape = match v with Arg _ -> [ `Shape `Circle ] | _ -> [] in
      let label = [ `Label (vertex_label v) ] in
      shape @ label

    let get_subgraph _ = None

    let default_edge_attributes _ = []

    let edge_attributes _ = []

    include Graph.Persistent.Digraph.ConcreteBidirectional (V)
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

let rec n_cartesian_product = function
  | [] -> [ [] ]
  | h :: t ->
      let rest = n_cartesian_product t in
      List.concat (List.map ~f:(fun i -> List.map ~f:(fun r -> i :: r) rest) h)

let to_contexts term args =
  let new_term, holes = Grammar.with_holes term in
  let ctxs =
    List.group_by (module String) (fun (n, _) -> n.V.symbol) args
    |> List.map ~f:(fun (sym, vals) ->
           let vals = List.map vals ~f:Tuple.T2.get2
           and holes =
             List.filter holes ~f:(fun (sym', _) -> String.(sym = sym'))
             |> List.map ~f:(fun (_, x) -> x)
           in
           Combinat.Permutation.Of_list.(to_list (create vals))
           |> List.map ~f:(fun vs -> List.zip_exn holes vs))
    |> n_cartesian_product
    |> List.map ~f:(fun ctxs ->
           List.concat ctxs |> Map.of_alist_exn (module String))
  in
  (* Every context should have a binding for every nonterminal. *)
  List.iter ctxs ~f:(fun ctx ->
      [%test_result: int]
        ~message:
          ( [%sexp_of: Grammar.Term.t * (V.state * _) list] (term, args)
          |> Sexp.to_string_hum )
        ~expect:(Grammar.Term.non_terminals term |> List.length)
        (Map.length ctx));
  (new_term, ctxs)

module Make
    (Sketch : Sigs.SKETCH)
    (S : Sigs.CODE)
    (L : Sigs.LANG with type 'a code = 'a S.t)
    (C : Sigs.CACHE with type value = L.Value.t and type 'a code = 'a S.t) =
struct
  open S.Int
  open C
  module Gr = Grammar

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
        let term, ctxs = to_contexts term args in
        let found_target =
          List.fold_until ctxs ~init:(S.Bool.bool false) ~finish:Fun.id
            ~f:(fun found ctx ->
              match L.Value.(L.eval ctx term = of_code target) with
              | `Static true -> Stop (S.Bool.bool true)
              | `Static false -> Continue found
              | `Dyn b -> Continue S.Bool.(found || b))
        in
        S.ite found_target
          (fun () ->
            S.sseq
              ( S.print ([%sexp_of: Gr.Term.t] term |> Sexp.to_string_hum)
              :: List.map args ~f:(fun (n, v) -> reconstruct ctx v n) ))
          (fun () -> S.unit)
      in
      let check_all =
        List.fold_left args ~init:check_one ~f:(fun check node ->
            let state = V.to_state node in
            let sym = state.V.symbol and size = int state.V.cost in
            fun ctx ->
              cache_iter ~sym ~size
                ~f:(fun v -> check ((state, v) :: ctx))
                cache)
      in
      check_all []

    (** Reconstruct a target assuming that it was produced by a particular code
      node. *)
    and of_code ({ graph = g; _ } as ctx) target code_node =
      let term = (V.to_code code_node).term and args = G.succ g code_node in
      if List.is_empty args then of_args ctx target term []
      else
        List.map args ~f:(fun n -> of_args ctx target term (G.succ g n))
        |> S.sseq

    and of_state ({ graph = g; _ } as ctx) state target =
      G.succ g (State state) |> List.map ~f:(of_code ctx target) |> S.sseq

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

  let search_graph ?(prune = true) max_cost =
    (* Compute search graph. *)
    let fresh = Fresh.create () and g = ref G.empty in

    for cost = 0 to max_cost do
      List.filter L.grammar ~f:(fun (_, rhs) -> Int.(Gr.Term.size rhs <= cost))
      |> List.iter ~f:(fun (lhs, rhs) ->
             let lhs_v = V.State { cost; symbol = lhs }
             and rhs_v = V.Code { cost; term = rhs } in
             g := G.add_edge !g lhs_v rhs_v;

             let n_holes = Gr.Term.n_holes rhs and size = Gr.Term.size rhs in
             Combinat.Partition.(
               iter
                 Int.(create ~n:(cost - size + n_holes) ~parts:n_holes)
                 ~f:(fun costs ->
                   let arg = V.Arg { id = Fresh.int fresh; n_args = n_holes } in
                   Gr.Term.non_terminals rhs
                   |> List.iteri ~f:(fun i sym ->
                          g := G.add_edge !g rhs_v arg;
                          g :=
                            G.add_edge !g arg
                              (V.State { cost = costs.{i}; symbol = sym })))))
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

  let fill_code g state code_node =
    let code = V.to_code code_node in
    let term = code.V.term
    and symbol = state.V.symbol
    and cost = state.V.cost in

    let insert value =
      S.sseq
        [
          debug_print
            (sprintf "Inserting (%s -> %s) cost %d" symbol
               (Gr.Term.to_string term) cost);
          put ~sym:symbol ~size:cost (cache.value ()) value;
        ]
    in

    let reconstruct value =
      (* Don't try to reconstruct a value that isn't of the same kind as the
         expected output. *)
      if String.(Sketch.output <> symbol) then (
        Log.debug (fun m ->
            m "Skipping reconstruction: (%s <> %s)" Sketch.output symbol);
        S.unit )
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
    in

    let fill args =
      let term, ctxs = to_contexts term args in
      List.map ctxs ~f:(fun ctx ->
          vlet (L.eval ctx term) @@ fun value ->
          S.seq (reconstruct value) (insert value))
      |> S.sseq
    in

    let args = G.succ g code_node in
    if List.is_empty args then [ fill [] ]
    else
      List.map args ~f:(fun arg_node ->
          let fill =
            G.succ g arg_node
            |> List.fold_left ~init:fill ~f:(fun fill node ->
                   let state = V.to_state node in
                   let symbol = state.symbol in
                   let cost = state.cost in
                   let fill ctx =
                     cache_iter ~sym:symbol ~size:(int cost)
                       ~f:(fun v -> fill ((state, v) :: ctx))
                       (cache.value ())
                   in
                   fill)
          in
          fill [])

  let fill_state g state =
    Log.debug (fun m -> m "Generating code for %s:%d" state.V.symbol state.cost);
    G.succ g (V.State state) |> List.concat_map ~f:(fill_code g state) |> S.sseq

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
