include struct
  open Dumb_params

  let spec = Spec.create ()

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

  let bank_size = Spec.add spec @@ Param.float_ref ~name:"bank-size" ()

  let final_value_dist = Spec.add spec @@ Param.float_ref ~name:"final-value-dist" ()

  let final_program_dist = Spec.add spec @@ Param.float_ref ~name:"final-program-dist" ()

  let program_cost = Spec.add spec @@ Param.float_ref ~name:"program-cost" ()

  let found_program = Spec.add spec @@ Param.bool_ref ~name:"found-program" ()

  let closest_program = Spec.add spec @@ Param.float_ref ~name:"closest-program" ()

  let have_parts = Spec.add spec @@ Param.float_ref ~name:"have-parts" ()

  let total_parts = Spec.add spec @@ Param.float_ref ~name:"total-parts" ()

  let synth = Spec.add spec @@ Param.const_str ~name:"synth" "sampling-diverse"

  let value_dist = Spec.add spec @@ Param.float_list ~json:false ~name:"value-dist" ()

  let max_cost = Spec.add spec @@ Param.int ~name:"max-cost" ~doc:" max search cost" ()

  let local_search_param ~name ?(json = true) () =
    (module struct
      type t = [ `Bounded | `Stochastic ] [@@deriving sexp_of]

      let key = Univ_map.Key.create ~name [%sexp_of: t]

      let name = name

      let of_string = function
        | "bounded" -> `Bounded
        | "stochastic" -> `Stochastic
        | kind -> raise_s [%message "unexpected" (kind : string)]

      let to_string = function `Bounded -> "bounded" | `Stochastic -> "stochastic"

      let arg_type = Command.Arg_type.create of_string

      let init =
        First
          (let open Command.Param in
          let param =
            flag_optional_with_default_doc name ~doc:" kind of local search" ~default:`Bounded arg_type sexp_of_t
          in
          map param ~f:(fun v -> Univ_map.Packed.T (key, v)))

      let to_json = if json then Option.return @@ fun v -> `String (to_string v) else None
    end : Param.S
      with type t = [ `Bounded | `Stochastic ])

  let local_search = Spec.add spec @@ Param.create @@ local_search_param ~name:"local" ()
end

module Make (Lang : Lang_intf.S) = struct
  open Lang
  module Search_state = Search_state_append.Make (Lang)
  open Search_state
  include Synth_utils.Generate_list (Lang)

  let generate_states = generate_states Search_state.search

  exception Done of Op.t Program.t

  let dedup_states ss states =
    states
    |> List.filter ~f:(fun (s, _, _) -> not (mem ss s))
    |> List.dedup_and_sort ~compare:(fun (s, _, _) (s', _, _) -> [%compare: Value.t] s s')

  let sample_diverse params ss min_dist new_states =
    Dumb_progress.List.map ~name:"sampling" new_states ~f:(fun ((_, new_state, _, _) as x) ->
        let min_dist =
          List.map (states ss) ~f:(fun old_state -> Value.dist params old_state new_state)
          |> List.min_elt ~compare:[%compare: float]
          |> Option.value ~default:Float.infinity
        in
        (min_dist, x))
    |> List.filter ~f:(fun (d, _) -> Float.(d >= min_dist))
    |> List.map ~f:Tuple.T2.get2

  let sample_naive _ new_states = new_states

  let sample_states params ss = if Params.get params diversity then sample_diverse params ss else sample_naive

  let insert_states params ss cost states =
    List.iter states ~f:(fun (_, state, op, args) -> insert ss cost state op args);
    Params.get params bank_size := Float.of_int @@ Search_state.length ss

  let search_bounded params ball_width ops center output cost f =
    let check_program p =
      if Program.size p >= cost then
        let v = Program.eval (Value.eval params) p in
        if Value.equal v output then f p
    in

    try Tree_ball.Rename_insert_delete.ball (module Op) ops center ball_width check_program
    with Program.Eval_error e -> raise @@ Program.Eval_error [%message (center : Op.t Program.t) (e : Sexp.t)]

  let search_stochastic params ops center output _ f =
    try
      Tree_ball.Rename_insert_delete.stochastic
        (module Op)
        ~score:(fun p -> 1.0 -. (Value.dist params output @@ Program.eval (Value.eval params) p))
        ops center
        (fun p s ->
          if Float.(s = 1.0) then
            let v = Program.eval (Value.eval params) p in
            if [%compare.equal: Value.t] v output then f p)
    with Program.Eval_error e -> raise @@ Program.Eval_error [%message (center : Op.t Program.t) (e : Sexp.t)]

  let search_neighbors params =
    match Params.get params local_search with
    | `Bounded -> search_bounded params (Params.get params ball_width)
    | `Stochastic -> search_stochastic params

  let synth params =
    let max_cost = Params.get params max_cost in
    let ss = Search_state.create max_cost in
    let bench = Params.get params bench in
    let ops = Bench.ops bench and output = Bench.output bench in

    let search_thresh = Params.get params search_thresh
    and retain_thresh = Params.get params retain_thresh
    and final_value_dist = Params.get params final_value_dist
    and final_program_dist = Params.get params final_program_dist
    and program_cost = Params.get params program_cost
    and found_program = Params.get params found_program
    and value_dist = Params.get params value_dist in

    let search_neighbors = search_neighbors params in
    try
      for cost = 0 to max_cost do
        let new_states = generate_states params ss ops cost |> dedup_states ss in

        let new_states = List.map new_states ~f:(fun (s, op, args) -> (Value.dist params output s, s, op, args)) in

        Queue.enqueue value_dist @@ List.map new_states ~f:(fun (d, _, _, _) -> d);

        (* Check neighbors around new states *)
        List.iter new_states ~f:(fun (d, _, op, args) ->
            if Float.(d <= search_thresh) then (
              let center = program_of_op_args_exn ss op args in

              search_neighbors ops center output cost (fun p ->
                  final_value_dist := d;
                  final_program_dist := Float.of_int @@ Tree_dist.zhang_sasha ~eq:[%compare.equal: Op.t] center p;
                  program_cost := Float.of_int cost;
                  found_program := true;
                  raise (Done p));
              print_s [%message "search failed"]));

        let new_states = sample_states params ss retain_thresh new_states in
        insert_states params ss cost new_states;

        Fmt.epr "Finished cost %d\n%!" cost;
        print_stats ss
      done
    with Done p ->
      assert (Value.equal (Program.eval (Value.eval params) p) output);
      eprint_s [%message (p : Op.t Program.t)]
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
