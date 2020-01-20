open! Core
module Fresh = Utils.Fresh
open Utils.Collections

let for_all a f =
  let rec loop i =
    if i >= Bigarray.Array1.dim a then true else f a.{i} && loop (i + 1)
  in
  loop 0

let to_list a = List.init (Bigarray.Array1.dim a) ~f:(fun i -> a.{i})

module V = struct
  type state = { cost : int; symbol : string } [@@deriving compare, hash, sexp]

  type code = { cost : int; term : string Grammar.Term.t }
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
           Combinat.Permutation.Of_list.to_list vals
           |> List.map ~f:(fun vs -> List.zip_exn holes vs))
    |> n_cartesian_product
    |> List.map ~f:(fun ctxs ->
           List.concat ctxs |> Map.of_alist_exn (module String))
  in
  (* Every context should have a binding for every nonterminal. *)
  List.iter ctxs ~f:(fun ctx ->
      [%test_result: int]
        ~message:
          ( [%sexp_of: string Grammar.Term.t * (V.state * _) list] (term, args)
          |> Sexp.to_string_hum )
        ~expect:(Grammar.Term.non_terminals term |> List.length)
        (Map.length ctx));
  (new_term, ctxs)

let%expect_test "" =
  to_contexts (App ("x", [])) []
  |> [%sexp_of: string Grammar.Term.t * int Map.M(String).t list] |> print_s;
  [%expect {| ((App x ()) (())) |}]

let%expect_test "" =
  to_contexts
    (App ("access", [ Nonterm "I"; Nonterm "L" ]))
    [ ({ cost = 1; symbol = "I" }, 0); ({ cost = 1; symbol = "L" }, 1) ]
  |> [%sexp_of: string Grammar.Term.t * int Map.M(String).t list] |> print_s;
  [%expect {| ((App access ((App I0 ()) (App L1 ()))) (((I0 0) (L1 1)))) |}]

let%expect_test "" =
  to_contexts
    (App ("sum", [ Nonterm "I"; Nonterm "I" ]))
    [ ({ cost = 1; symbol = "I" }, 0); ({ cost = 1; symbol = "I" }, 1) ]
  |> [%sexp_of: string Grammar.Term.t * int Map.M(String).t list] |> print_s;
  [%expect
    {| ((App sum ((App I0 ()) (App I1 ()))) (((I0 0) (I1 1)) ((I0 1) (I1 0)))) |}]

let%expect_test "" =
  to_contexts
    (App ("sum3", [ Nonterm "I"; Nonterm "I"; Nonterm "J" ]))
    [
      ({ cost = 1; symbol = "I" }, 0);
      ({ cost = 1; symbol = "I" }, 1);
      ({ cost = 1; symbol = "J" }, 2);
    ]
  |> [%sexp_of: string Grammar.Term.t * int Map.M(String).t list] |> print_s;
  [%expect
    {|
    ((App sum3 ((App I0 ()) (App I1 ()) (App J2 ())))
     (((I0 0) (I1 1) (J2 2)) ((I0 1) (I1 0) (J2 2)))) |}]

module Make
    (Sketch : Sigs.SKETCH)
    (S : Sigs.CODE)
    (L : Sigs.LANG with type 'a code = 'a S.t and type type_ = S.ctype)
    (C : Sigs.CACHE with type value = L.Value.t and type 'a code = 'a S.t) =
struct
  open S.Int
  open C
  module Gr = Grammar

  let debug = true

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

  let costs_t = S.Array.mk_type S.int_t

  let rec reconstruct tbl g state target =
    let args_t = L.Value.type_of target in
    let func_t = S.Func.type_ args_t S.unit_t
    and func_name = sprintf "reconstruct_%s_%d" state.V.symbol state.cost in
    Log.debug (fun m ->
        m "Building %s :: %s." func_name
          ([%sexp_of: S.ctype] func_t |> Sexp.to_string));

    let recon_args target term args =
      let check args =
        let term, ctxs = to_contexts term args in
        let found_target =
          List.fold_left ctxs ~init:(S.Bool.bool false) ~f:(fun found ctx ->
              S.Bool.(found || L.Value.eq (L.eval ctx term) target))
        in
        S.ite found_target
          (fun () ->
            S.seq_many
              ( S.print ([%sexp_of: string Gr.Term.t] term |> Sexp.to_string_hum)
              :: List.map args ~f:(fun (n, v) -> reconstruct tbl g n v) ))
          (fun () -> S.unit)
      in
      let check =
        List.fold_left args ~init:check ~f:(fun check node ->
            let state = V.to_state node in
            let check ctx =
              C.iter ~sym:state.V.symbol ~size:(int state.V.cost)
                ~f:(fun (v, _) -> check ((state, v) :: ctx))
                tbl
            in
            check)
      in
      check []
    in

    let recon_code target code_node =
      let term = (V.to_code code_node).term and args = G.succ g code_node in
      if List.is_empty args then recon_args target term []
      else
        List.map args ~f:(fun args_node ->
            recon_args target term (G.succ g args_node))
        |> S.seq_many
    in

    let recon_state target =
      G.succ g (State state) |> List.map ~f:(recon_code target) |> S.seq_many
    in
    let func =
      S.func func_name func_t (fun target -> S.let_ target recon_state)
    in
    S.apply func (L.Value.code target)

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

    Log.debug (fun m -> m "%s" ([%sexp_of: Gr.t] L.grammar |> Sexp.to_string));
    for cost = 0 to max_cost do
      List.filter L.grammar ~f:(fun (_, rhs) -> Int.(Gr.Term.size rhs <= cost))
      |> List.iter ~f:(fun (lhs, rhs) ->
             let lhs_v = V.State { cost; symbol = lhs }
             and rhs_v = V.Code { cost; term = rhs } in
             g := G.add_edge !g lhs_v rhs_v;

             let n_holes = Gr.Term.n_holes rhs and size = Gr.Term.size rhs in
             Combinat.Partition.iter
               Int.(cost - size + n_holes, n_holes)
               ~f:(fun costs ->
                 let arg = V.Arg { id = Fresh.int fresh; n_args = n_holes } in
                 Gr.Term.non_terminals rhs
                 |> List.iteri ~f:(fun i sym ->
                        g := G.add_edge !g rhs_v arg;
                        g :=
                          G.add_edge !g arg
                            (V.State { cost = costs.{i}; symbol = sym }))))
    done;
    let g = !g in

    let sinks =
      G.fold_vertex
        (fun v vs ->
          match v with
          | State { symbol; _ } when String.(symbol = Sketch.output) -> v :: vs
          | _ -> vs)
        g []
    and sources =
      List.mapi Sketch.inputs ~f:(fun i _ ->
          V.Code { term = App (sprintf "i%d" i, []); cost = 1 })
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

  let is_inputs_loaded = ref false

  let load_inputs tbl =
    is_inputs_loaded := true;
    let open S in
    let_ (Sexp.to_list Sexp.input) @@ fun sexp_inputs ->
    for_ (int 0) (int 1) (Sexp.List.length sexp_inputs) (fun i ->
        let_ (Sexp.List.get sexp_inputs i |> Sexp.to_list)
        @@ fun sexp_sym_val ->
        let_ (Sexp.List.get sexp_sym_val (int 0) |> String.of_sexp)
        @@ fun sym ->
        L.Value.let_ (Sexp.List.get sexp_sym_val (int 1) |> L.Value.of_sexp)
        @@ fun value ->
        List.map (Gr.non_terminals L.grammar) ~f:(fun sym' ->
            ite
              String.(O.(const sym' = sym))
              (fun () ->
                put ~sym:sym' ~size:1
                  ~sizes:(S.Array.const costs_t [||])
                  tbl value)
              (fun () -> unit))
        |> seq_many)

  let output () =
    let open S in
    let output =
      lazy
        ( if not !is_inputs_loaded then
            failwith "Inputs must be loaded before output.";
          genlet (Sexp.input |> L.Value.of_sexp) )
    in
    Lazy.force output

  let fill_code tbl g state code_node =
    let code = V.to_code code_node in
    let term = code.V.term
    and symbol = state.V.symbol
    and cost = state.V.cost in

    let insert value =
      S.seq_many
        [
          debug_print
            (sprintf "Inserting (%s -> %s) cost %d" symbol
               (Gr.Term.to_string term) cost);
          put ~sym:symbol ~size:cost
            ~sizes:(S.Array.const costs_t [||])
            tbl value;
        ]
    in
    let fill args =
      let term, ctxs = to_contexts term args in
      List.map ctxs ~f:(fun ctx ->
          L.Value.let_ (L.eval ctx term) @@ fun value ->
          let recon_code =
            match L.Value.(value = output ()) with
            | Some eq when String.(Sketch.output = symbol) ->
                S.ite eq
                  (fun () ->
                    S.seq_many
                      [
                        S.print "Starting reconstruction";
                        ( S.let_ (reconstruct tbl g state (output ()))
                        @@ fun _ -> S.exit );
                      ])
                  (fun () -> S.unit)
            | _ -> S.unit
          in
          S.seq recon_code (insert value))
      |> S.seq_many
    in

    let args = G.succ g code_node in
    if List.is_empty args then [ fill [] ]
    else
      List.map args ~f:(fun arg_node ->
          let fill =
            G.succ g arg_node
            |> List.fold_left ~init:fill ~f:(fun fill node ->
                   let state = V.to_state node in
                   let fill ctx =
                     C.iter ~sym:symbol ~size:(int cost)
                       ~f:(fun (v, _) -> fill ((state, v) :: ctx))
                       tbl
                   in
                   fill)
          in
          fill [])

  let fill_state tbl g state =
    Log.debug (fun m -> m "Generating code for %s:%d" state.V.symbol state.cost);
    G.succ g (V.State state)
    |> List.concat_map ~f:(fill_code tbl g state)
    |> S.seq_many

  let enumerate max_cost =
    let g = search_graph max_cost in

    (* Generate a graph that only contains the state nodes. *)
    let lhs_g = contract_state g |> G.reverse in

    C.empty (fun tbl ->
        (* Fold over the state nodes, generating code to fill them. *)
        let loops =
          G.Topo.fold
            (fun v code ->
              let state = V.to_state v in
              code @ [ fill_state tbl g state ])
            lhs_g []
        in
        S.seq_many loops)
end
