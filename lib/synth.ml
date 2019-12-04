open! Core
module Fresh = Utils.Fresh
open Utils.Collections

let for_all a f =
  let rec loop i =
    if i >= Bigarray.Array1.dim a then true else f a.{i} && loop (i + 1)
  in
  loop 0

let to_list a = List.init (Bigarray.Array1.dim a) ~f:(fun i -> a.{i})

module Make
    (S : Sigs.CODE)
    (L : Sigs.LANG with type 'a code = 'a S.t and type type_ = S.ctype)
    (C : Sigs.CACHE with type value = L.value and type 'a code = 'a S.t) =
struct
  open C
  module Gr = Grammar

  let debug = false

  let debug_print msg = if debug then S.print msg else S.unit

  let rec of_list = function
    | [] -> S.unit
    | [ l ] -> l
    | l :: ls -> S.seq l (of_list ls)

  let rec case pred default = function
    | [] -> default
    | (v, k) :: bs -> S.ite (pred v) k (case pred default bs)

  let rec seq_many = function [] -> S.unit | x :: xs -> S.seq x (seq_many xs)

  let rec let_many f = function
    | [] -> f []
    | [ x ] -> S.let_ x (fun x -> f [ x ])
    | x :: xs -> S.let_ x (fun x -> let_many (fun xs -> f (x :: xs)) xs)

  let costs_t = S.Array.mk_type Int

  let rec reconstruct tbl sym costs output =
    let fresh = Fresh.create () in
    let args_t = S.Tuple.mk_type costs_t (L.type_of output) in
    let func_t = S.Func (args_t, Unit) in
    let func_name = sprintf "reconstruct_%s" sym in
    Log.debug (fun m ->
        m "Building %s :: %s." func_name
          ([%sexp_of: S.ctype] func_t |> Sexp.to_string));
    let body costs target =
      Gr.rhs L.grammar sym
      |> List.map ~f:(Gr.with_holes ~fresh L.grammar)
      |> List.group_by (module Int) (fun (_, hs) -> List.length hs)
      |> List.map ~f:(fun (n_holes, rhss) ->
             let case = S.int n_holes in
             let costs = List.init n_holes ~f:(fun i -> S.(costs.(int i))) in
             let code : unit S.t =
               let_many
                 (fun costs ->
                   List.map rhss ~f:(fun (term, holes) ->
                       let hole_costs = List.zip_exn costs holes in
                       let check ctx =
                         let ectx = Map.map ctx ~f:(fun (v, _) -> v) in
                         let v = L.eval ectx term in
                         S.ite (L.eq v target)
                           (seq_many
                              ( S.print
                                  ( [%sexp_of: Gr.Term.t] term
                                  |> Sexp.to_string_hum )
                              :: List.map hole_costs ~f:(fun (_, (sym, name)) ->
                                     let target, costs =
                                       Map.find_exn ctx name
                                     in
                                     reconstruct tbl sym costs target) ))
                           S.unit
                       in
                       let check =
                         List.fold_left hole_costs ~init:check
                           ~f:(fun check (cost, (sym, name)) ->
                             let check ctx =
                               C.iter ~sym ~size:cost
                                 ~f:(fun v ->
                                   check (Map.add_exn ctx ~key:name ~data:v))
                                 tbl
                             in
                             check)
                       in
                       check (Map.empty (module String)))
                   |> of_list)
                 costs
             in
             (case, code))
      |> case (fun size -> S.(Array.length costs = size)) S.unit
    in
    let func =
      S.func func_name func_t (fun tup ->
          S.let_ (S.Tuple.fst tup) (fun costs ->
              S.let_ (S.Tuple.snd tup) (fun target -> body costs target)))
    in
    S.apply func (S.Tuple.create costs (L.code output))

  let put_all tbl ctx cost costs lhs rhs =
    let open S in
    let v = L.eval (Map.of_alist_exn (module String) ctx) rhs in
    let costs =
      Array.const costs_t (costs |> List.map ~f:int |> List.to_array)
    in
    let insert_code =
      seq_many
        [
          debug_print
            (sprintf "Inserting (%s -> %s) cost %d" lhs (Gr.Term.to_string rhs)
               cost);
          put ~sym:lhs ~size:cost ~sizes:costs tbl v;
        ]
    in
    let out_sym, out_val = L.output in
    if String.(out_sym = lhs) then
      match L.(v = out_val) with
      | Some eq ->
          let recon_code =
            seq_many
              [
                print "Starting reconstruction";
                reconstruct tbl lhs costs out_val;
                exit;
              ]
          in
          ite eq recon_code insert_code
      | None -> insert_code
    else insert_code

  let put_leaf_rule tbl cost (lhs, rhs) = [ put_all tbl [] cost [] lhs rhs ]

  let put_rule tbl cost holes made ((lhs, rhs) as rule) =
    Combinat.Partition.fold
      (cost - Gr.rule_size rule + List.length holes, List.length holes)
      ~init:[]
      ~f:(fun code costs ->
        if for_all costs (Set.mem made) then (
          Log.debug (fun m ->
              m "Enumerating (%s -> %s) at cost %d" lhs (Gr.Term.to_string rhs)
                cost);
          let loop =
            let put_all ctx = put_all tbl ctx cost (to_list costs) lhs rhs in
            List.foldi holes ~init:put_all ~f:(fun i put_all (sym, name) ->
                let put_all ctx =
                  C.iter ~sym
                    ~size:(S.int costs.{i})
                    ~f:(fun (v, _) -> put_all ((name, v) :: ctx))
                    tbl
                in
                put_all)
          in
          loop [] :: code )
        else (
          Log.debug (fun m ->
              m "Ignoring (%s -> %s) at cost %d" lhs (Gr.Term.to_string rhs)
                cost);
          code ))

  let enumerate_rule tbl made cost ((lhs, rhs) as rule) =
    let fresh = Fresh.create () in
    let rhs, holes = Gr.with_holes ~fresh L.grammar rhs in
    let n_holes = List.length holes in
    if n_holes = 0 && Gr.rule_size rule = cost then (
      Log.debug (fun m ->
          m "Enumerating (%s -> %s) at cost %d" lhs (Gr.Term.to_string rhs) cost);
      put_leaf_rule tbl cost rule )
    else put_rule tbl cost holes made (lhs, rhs)

  let enumerate_cost tbl made cost =
    List.filter L.grammar ~f:(fun rule -> Gr.rule_size rule <= cost)
    |> List.concat_map ~f:(enumerate_rule tbl made cost)

  let enumerate max_cost =
    C.empty (fun tbl ->
        let _, loops =
          List.init max_cost ~f:(fun c -> c)
          |> List.fold_left
               ~init:(Set.empty (module Int), [])
               ~f:(fun (made, loops) cost ->
                 let loops' =
                   enumerate_cost tbl made cost
                   @ [ S.print (sprintf "Completed %d/%d" cost max_cost) ]
                 in
                 if List.length loops' > 0 then
                   (Set.add made cost, loops @ loops')
                 else (made, loops))
        in
        of_list loops)

  module V = struct
    type t = { cost : int; kind : [ `Lhs of string | `Rhs of Gr.Term.t ] }
    [@@deriving compare, hash, sexp]

    let equal = [%compare.equal: t]
  end

  module G = struct
    module X = struct
      let graph_attributes _ = []

      let default_vertex_attributes _ = []

      let vertex_name V.{ cost; kind } =
        match kind with
        | `Lhs sym -> sprintf "\"%s : %d\"" sym cost
        | `Rhs term -> sprintf "\"%s : %d\"" (Gr.Term.to_string term) cost

      let vertex_attributes _ = []

      let get_subgraph _ = None

      let default_edge_attributes _ = []

      let edge_attributes _ = []

      include Graph.Imperative.Digraph.ConcreteBidirectional (V)
    end

    include X
    include Graph.Graphviz.Dot (X)
    include Graph.Traverse.Dfs (X)
    include Graph.Oper.I (X)
  end

  let search_graph max_cost =
    (* Compute search graph. *)
    let g = G.create () in
    for cost = 0 to max_cost do
      List.filter L.grammar ~f:(fun rule -> Gr.rule_size rule <= cost)
      |> List.iter ~f:(fun ((lhs, rhs) as rule) ->
             let lhs_v = V.{ cost; kind = `Lhs lhs } in
             let rhs_v = V.{ cost; kind = `Rhs rhs } in
             G.add_vertex g lhs_v;
             G.add_vertex g rhs_v;
             G.add_edge g lhs_v rhs_v;

             let fresh = Fresh.create () in
             let _, holes = Gr.with_holes ~fresh L.grammar rhs in
             let n_holes = List.length holes in
             if n_holes = 0 && Gr.rule_size rule = cost then ()
             else
               Combinat.Partition.iter
                 ( cost - Gr.rule_size rule + List.length holes,
                   List.length holes )
                 ~f:(fun costs ->
                   List.iteri holes ~f:(fun i (sym, _) ->
                       let v = V.{ cost = costs.{i}; kind = `Lhs sym } in
                       G.add_vertex g v;
                       G.add_edge g rhs_v v)))
    done;

    (* Prune nodes that the sinks don't depend on. *)
    let g_trans = G.transitive_closure g in
    let out_sym, _ = L.output in
    let sinks =
      G.fold_vertex
        (fun ({ kind; _ } as v) vs ->
          match kind with
          | `Lhs sym when String.(sym = out_sym) -> v :: vs
          | _ -> vs)
        g []
    in
    let sources =
      G.fold_vertex
        (fun ({ kind; _ } as v) vs ->
          if List.exists L.inputs ~f:(fun (sym, _) -> Poly.(kind = `Lhs sym))
          then v :: vs
          else vs)
        g []
    in

    G.iter_vertex
      (fun v ->
        let should_remove =
          not
            ( ( List.mem sinks v ~equal:V.equal
              || List.exists sinks ~f:(fun v' -> G.mem_edge g_trans v' v) )
            && ( List.mem sources v ~equal:V.equal
               || List.exists sources ~f:(fun v' -> G.mem_edge g_trans v v') )
            )
        in
        if should_remove then G.remove_vertex g v)
      g;

    g
end
