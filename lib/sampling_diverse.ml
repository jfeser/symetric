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

  let synth = Spec.add spec @@ Param.const_str ~name:"synth" "sampling-diverse"

  let local_search =
    Spec.add spec
    @@ Param.symbol ~name:"local" ~doc:" kind of local search" ~default:`Bounded
         [ (`Bounded, "bounded"); (`Stochastic, "stochastic") ]
end

module Make (Lang : Lang_intf.S) = struct
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

  let sample_pairwise dist retain_thresh states old new_ =
    Dumb_progress.List.map ~name:"sampling" new_ ~f:(fun new_idx ->
        let min_dist =
          List.map old ~f:(fun old_idx -> dist states.(old_idx) states.(new_idx))
          |> List.min_elt ~compare:[%compare: float]
          |> Option.value ~default:Float.infinity
        in
        (min_dist, new_idx))
    |> List.filter ~f:(fun (d, _) -> Float.(d >= retain_thresh))
    |> List.map ~f:Tuple.T2.get2

  class synthesizer params =
    let _bench = Params.get params bench in
    object (self)
      inherit Parent.synthesizer params as super

      val search_thresh = Params.get params search_thresh

      val diversity = Params.get params diversity

      val mutable retain_thresh = Params.get params retain_thresh

      val search_neighbors =
        match Params.get params local_search with
        | `Bounded -> bounded_search params _bench (Params.get params ball_width)
        | `Stochastic -> stochastic_search params _bench

      method distance = Value.dist params

      method dedup_states states =
        states
        |> List.filter ~f:(fun (s, _, _) -> not (Search_state.mem search_state s))
        |> List.dedup_and_sort ~compare:(fun (s, _, _) (s', _, _) -> [%compare: Value.t] s s')

      method find_close_states new_states =
        List.filter_map new_states ~f:(fun (v, op, args) ->
            let d = self#distance v _output in
            if Float.(d <= search_thresh) then Some (d, v, op, args) else None)

      method search_close_states new_states =
        self#find_close_states new_states
        |> List.iter ~f:(fun (d, _, op, args) ->
               print_s [%message "searching"];
               let center = Search_state.program_of_op_args_exn search_state op args in

               search_neighbors self center @@ fun p ->
               let final_value_dist = Params.get params final_value_dist
               and final_program_dist = Params.get params final_program_dist in
               final_value_dist := d;
               final_program_dist := Float.of_int @@ Tree_dist.zhang_sasha ~eq:[%compare.equal: Op.t] center p;
               raise (Parent.Done p))
      (** Check neighbors around new states *)

      method sample_diverse_states new_states =
        let new_states_a = Array.of_list new_states in
        let all_states =
          Array.of_list @@ List.map new_states ~f:(fun (v, _, _) -> v) @ Search_state.states search_state
        in
        let old = List.range (List.length new_states) (Array.length all_states)
        and new_ = List.range 0 (List.length new_states) in
        sample_pairwise self#distance retain_thresh all_states old new_ |> List.map ~f:(fun i -> new_states_a.(i))

      method sample_states new_states =
        let sample = self#sample_diverse_states new_states in
        if diversity then sample else List.take (List.permute new_states) (List.length sample)

      method! generate_states cost =
        let new_states = super#generate_states cost |> self#dedup_states in
        self#search_close_states new_states;
        self#sample_states new_states

      method! run =
        let rec reduce_retain_thresh () =
          match super#run with
          | Some p -> Some p
          | None ->
              (* TODO: Should we clear the search space here? *)
              Search_state.clear search_state;
              retain_thresh <- retain_thresh /. 2.0;
              reduce_retain_thresh ()
        in
        reduce_retain_thresh ()
    end

  let synth params = Option.iter (new synthesizer params)#run ~f:(fun p -> eprint_s [%message (p : Op.t Program.t)])
end

let cli (type value op) (module Lang : Lang_intf.S with type Value.t = value and type Op.t = op) =
  let module Synth = Make (Lang) in
  let spec = Dumb_params.Spec.union [ Lang.spec; Params.spec; spec ] in
  let open Command.Let_syntax in
  Command.basic
    ~summary:(sprintf "Diversity sampling for %s" Lang.name)
    [%map_open
      let params = Dumb_params.Spec.cli spec in
      Synth_utils.run_synth Synth.synth params]
