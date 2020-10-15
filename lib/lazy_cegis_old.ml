[@@@landmark "auto"]

open Search_state

module Seq = struct
  include Sequence

  let of_array a = init (Array.length a) ~f:(fun i -> a.(i))
end

module Program = struct
  module T = struct
    type t = [ `Apply of Op.t * t list ] [@@deriving compare, hash, sexp]
  end

  let rec ceval (`Apply (op, args)) =
    match (op, args) with
    | Op.Input x, _ -> x
    | Union, [ x; y ] -> Array.map2_exn (ceval x) (ceval y) ~f:( || )
    | Inter, [ x; y ] -> Array.map2_exn (ceval x) (ceval y) ~f:( && )
    | Sub, [ x; y ] ->
        Array.map2_exn (ceval x) (ceval y) ~f:(fun a b -> a && not b)
    | _ -> assert false

  let rec size (`Apply (_, args)) = 1 + List.sum (module Int) args ~f:size

  include T
  include Comparator.Make (T)
end

let did_change f =
  G.reset_changed ();
  let ret = f () in
  let changed = G.has_changed () in
  (changed, ret)

let until_done f =
  let rec until_done did_work =
    let did_work' = f () in
    if did_work' then until_done did_work' else did_work
  in
  until_done false

let fill_cost (graph : Search_state.t) cost =
  Fmt.epr "Filling cost %d\n" cost;
  let size = nb_vertex graph in
  if cost > 1 then (
    let arg_cost = cost - 1 in
    let module Comp = Combinat.Composition in
    Comp.create ~n:arg_cost ~k:2
    |> Comp.iter ~f:(fun arg_costs ->
           let c = arg_costs.{0} and c' = arg_costs.{1} in
           states_of_cost graph c
           |> List.iter ~f:(fun (a : State.t) ->
                  states_of_cost graph c'
                  |> List.iter ~f:(fun (a' : State.t) ->
                         List.iter [ Op.Union; Inter; Sub ] ~f:(fun op ->
                             let state =
                               let s = State.state a and s' = State.state a' in
                               match op with
                               | Op.Union -> Abs.union s s'
                               | Inter -> Abs.inter s s'
                               | Sub -> Abs.sub s s'
                               | _ -> assert false
                             in
                             let state_v_out =
                               State.create state cost |> Is_fresh.unwrap
                             in
                             insert_hyper_edge_if_not_exists graph [ a; a' ] op
                               state_v_out))));

    let size' = nb_vertex graph in
    Dump.dump_detailed ~suffix:(sprintf "after-fill-%d" cost) graph;

    Fmt.epr "Pruning: size before=%d, after=%d, removed %f%%\n" size size'
      Float.(100.0 - (of_int size' / of_int size * 100.0)) )

let fill_up_to_cost (graph : Search_state.t) cost =
  let rec fill c =
    if c > cost then false
    else
      let changed, () = did_change @@ fun () -> fill_cost graph c in
      changed || fill (c + 1)
  in
  fill 1

module Stats = struct
  type t = {
    n_state_nodes : int;
    n_arg_nodes : int;
    n_covered : int;
    n_refuted : int;
    min_width : int;
    max_width : int;
    median_width : int;
    sat : bool;
  }
end

exception Done of [ `Sat | `Unsat ]

let separators graph target =
  Seq.unfold ~init:(G.succ graph target) ~f:(fun sep ->
      if List.is_empty sep then None
      else
        let sep' =
          List.concat_map sep ~f:(G.succ graph)
          |> List.concat_map ~f:(G.succ graph)
        in
        Some (sep, sep'))

let refine_level n graph =
  V.fold graph ~init:(0, 0) ~f:(fun ((num, dem) as acc) ->
      Node.match_
        ~args:(fun _ -> acc)
        ~state:(fun v -> (num + Abs.width (State.state v), dem + n)))

let refine search_state output refinement =
  let graph = search_state in
  let size = nb_vertex search_state in

  List.iteri refinement ~f:(fun i (r : Refine.Refinement.t) ->
      List.iter r.splits ~f:(fun (state_v, refined_states) ->
          G.remove_edge graph (Node.of_state state_v) (Node.of_args r.context);

          let cost = State.cost state_v in
          List.iter refined_states ~f:(fun state ->
              let (Fresh v' | Stale v') = State.create state cost in
              G.add_edge_e graph (Node.of_state v', -1, Node.of_args r.context)));

      Dump.dump_detailed ~output ~suffix:(sprintf "fixup-%d" i) graph);

  Dump.dump_detailed ~output ~suffix:"before-fixup" graph;
  fix_up search_state;
  Dump.dump_detailed ~output ~suffix:"after-fixup" graph;

  let size' = nb_vertex search_state in
  Fmt.epr "Pruning: size before=%d, after=%d, removed %f%%\n" size size'
    Float.(100.0 - (of_int size' / of_int size * 100.0));

  let num, dem = refine_level (Array.length output) graph in
  Fmt.epr "Refine level: %d/%d (%f%%)\n" num dem
    Float.(of_int num / of_int dem * 100.0)

let rec extract_program graph selected_edges target =
  let args =
    G.succ_e graph target
    |> List.filter ~f:(Set.mem selected_edges)
    |> List.map ~f:(fun (_, _, v) -> v)
    |> List.map ~f:Node.to_args_exn
  in
  match args with
  | [ a ] ->
      `Apply
        ( Args.op a,
          G.succ graph (Node.of_args a)
          |> List.map ~f:(extract_program graph selected_edges) )
  | args ->
      Error.create "Too many args" args [%sexp_of: Args.t list] |> Error.raise

let refute search_state output =
  let graph = search_state in
  match
    V.find_map graph ~f:(fun v ->
        match Node.to_state v with
        | Some v when Abs.contains (State.state v) output -> Some v
        | _ -> None)
  with
  | Some target ->
      let seps = separators graph (Node.of_state target) |> Seq.to_list in
      let seps, last_sep =
        match List.rev seps with
        | last :: rest -> (List.rev rest, last)
        | _ -> failwith "No separators"
      in

      Dump.dump_detailed ~output ~suffix:"before-refinement" ~depth:0 graph;

      let refinement =
        List.find_map seps ~f:(fun sep ->
            let open Option.Let_syntax in
            let%map r =
              Refine.get_refinement search_state target output sep
              |> Either.First.to_option
            in
            (sep, r))
      in
      ( match refinement with
      | Some (_, r) -> refine search_state output r
      | None -> (
          match Refine.get_refinement search_state target output last_sep with
          | First r -> refine search_state output r
          | Second selected_edges ->
              Fmt.epr "Could not refute: %a" Sexp.pp_hum
                ( [%sexp_of: Program.t]
                @@ extract_program graph selected_edges (Node.of_state target)
                );
              raise (Done `Sat) ) );
      true
  | None -> false

let count_compressible graph =
  let args =
    V.filter_map graph ~f:Node.to_args
    |> List.map ~f:(fun v ->
           let v = Node.of_args v in
           (G.pred graph v, G.succ graph v))
  in
  let module Key = struct
    module T = struct
      type t = Node.t list * Node.t list [@@deriving compare, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end in
  let set_args = Set.of_list (module Key) args in
  Fmt.epr "Compressed args: reduces %d to %d\n" (List.length args)
    (Set.length set_args)

let synth ?(no_abstraction = false) inputs output =
  let search_state = create () in
  let graph = search_state in

  (* Add inputs to the state space graph. *)
  List.iter inputs ~f:(fun input ->
      let state = if no_abstraction then Abs.lift input else Abs.top in
      let state_v_out = State.create state 1 |> Is_fresh.unwrap in
      insert_hyper_edge_if_not_exists graph [] (Op.Input input) state_v_out);

  let status =
    try
      for cost = 1 to !Global.max_cost do
        until_done (fun () ->
            let changed = until_done (fun () -> refute search_state output) in
            let changed' = fill_up_to_cost search_state cost in
            Fmt.epr "Changed: %b Cost: %d\n%!" (changed || changed') cost;
            changed || changed')
        |> ignore
      done;
      `Unsat
    with Done status -> status
  in

  let widths =
    V.filter_map graph ~f:(fun v ->
        match Node.to_state v with
        | Some v -> Some (Abs.width @@ State.state v)
        | _ -> None)
    |> List.sort ~compare:[%compare: int]
    |> Array.of_list
  in
  let stats =
    Stats.
      {
        n_state_nodes = V.filter graph ~f:Node.is_state |> List.length;
        n_arg_nodes = V.filter graph ~f:Node.is_args |> List.length;
        n_covered = -1;
        n_refuted = !Global.n_refuted;
        min_width = widths.(0);
        max_width = widths.(Array.length widths - 1);
        median_width = widths.(Array.length widths / 2);
        sat = (match status with `Sat -> true | `Unsat -> false);
      }
  in
  (search_state, stats)

let sample ?(state = Random.State.default) inputs =
  let open Grammar in
  let named_inputs = List.mapi inputs ~f:(fun i x -> (sprintf "i%d" i, x)) in
  let input_rules =
    List.map named_inputs ~f:(fun (n, _) -> Rule.create "p" (Term.app n []) [])
  in
  let g =
    input_rules
    @ [
        Rule.create "p"
          (Term.app "and" [ Term.nonterm "p"; Term.nonterm "p" ])
          [];
        Rule.create "p"
          (Term.app "or" [ Term.nonterm "p"; Term.nonterm "p" ])
          [];
        Rule.create "p"
          (Term.app "diff" [ Term.nonterm "p"; Term.nonterm "p" ])
          [];
      ]
  in
  let rec to_prog = function
    | App (func, args) ->
        let op =
          match func with
          | "and" -> Op.Inter
          | "or" -> Union
          | "diff" -> Sub
          | _ -> (
              match
                List.Assoc.find ~equal:[%compare.equal: string] named_inputs
                  func
              with
              | Some i -> Input i
              | None -> failwith "unexpected function" )
        in
        let args = List.map args ~f:to_prog in
        `Apply (op, args)
    | _ -> failwith "unexpected term"
  in
  let rec sample_prog () =
    let p = to_prog (Grammar.sample ~state "p" g :> Untyped_term.t) in
    if Program.size p > !Global.max_cost then sample_prog () else p
  in
  sample_prog ()

let sample_big ?(state = Random.State.default) inputs =
  let open Grammar in
  let named_inputs = List.mapi inputs ~f:(fun i x -> (sprintf "i%d" i, x)) in
  let input_rules =
    List.map named_inputs ~f:(fun (n, _) -> Rule.create "p" (Term.app n []) [])
  in
  let g =
    input_rules
    @ [
        Rule.create "p"
          (Term.app "and" [ Term.nonterm "p"; Term.nonterm "p" ])
          [];
        Rule.create "p"
          (Term.app "or" [ Term.nonterm "p"; Term.nonterm "p" ])
          [];
        Rule.create "p"
          (Term.app "diff" [ Term.nonterm "p"; Term.nonterm "p" ])
          [];
      ]
  in
  let rec to_prog = function
    | App (func, args) ->
        let op =
          match func with
          | "and" -> Op.Inter
          | "or" -> Union
          | "diff" -> Sub
          | _ -> (
              match
                List.Assoc.find ~equal:[%compare.equal: string] named_inputs
                  func
              with
              | Some i -> Input i
              | None -> failwith "unexpected function" )
        in
        let args = List.map args ~f:to_prog in
        `Apply (op, args)
    | _ -> failwith "unexpected term"
  in
  let rec sample_prog () =
    let p = to_prog (Grammar.sample ~state "p" g :> Untyped_term.t) in
    if Program.size p <> !Global.max_cost then sample_prog () else p
  in
  sample_prog ()

let check_search_space ?(n = 100_000) inputs graph =
  let rec loop i =
    if i > n then (
      Fmt.epr "Checked %d programs and found no counterexamples\n" n;
      Ok () )
    else
      let prog = sample inputs in
      let cstate = Program.ceval prog in
      match
        V.find_map graph ~f:(fun v ->
            match Node.to_state v with
            | Some v when Abs.contains (State.state v) cstate -> Some v
            | _ -> None)
      with
      | Some _ -> loop (i + 1)
      | None ->
          Fmt.epr "Missed program %a with size %d and state %a\n" Sexp.pp
            ([%sexp_of: Program.t] prog)
            (Program.size prog) Conc.pp cstate;
          Error cstate
  in
  loop 0

let random_likely_unsat ?(state = Random.State.default) n k =
  let inputs =
    List.init n ~f:(fun _ -> Array.init k ~f:(fun _ -> Random.State.bool state))
  in
  let output = Array.init k ~f:(fun _ -> Random.State.bool state) in
  (inputs, output)

let random_sat ?(state = Random.State.default) n k =
  let inputs =
    List.init n ~f:(fun _ -> Array.init k ~f:(fun _ -> Random.State.bool state))
  in
  let output = sample_big ~state inputs |> Program.ceval in
  (inputs, output)

let random_io ?(state = Random.State.default) ~n ~k =
  (* if Random.State.bool state then random_sat ~state n k
   * else *)
  random_sat ~state n k
