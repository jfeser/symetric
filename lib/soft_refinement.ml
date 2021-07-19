include struct
  open Dumb_params

  let spec = Spec.create ()

  let per_cost =
    Spec.add spec
    @@ Param.int ~name:"per-cost" ~aliases:[ "p" ] ~doc:" programs stored per cost level" ~init:(`Cli (Some 100)) ()

  let thresh =
    Spec.add spec
    @@ Param.float ~name:"thresh" ~aliases:[ "d" ] ~doc:" exhaustive search threshold" ~init:(`Cli (Some 1.0)) ()

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

  let max_cost = Spec.add spec @@ Param.int ~name:"max-cost" ~doc:" max search cost" ()

  let local_search_param ~name ?(csv = true) () =
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

      let to_json = if csv then Option.return @@ fun v -> `String (to_string v) else None
    end : Param.S
      with type t = [ `Bounded | `Stochastic ])

  let local_search = Spec.add spec @@ Param.create @@ local_search_param ~name:"local" ()
end

module Make (Lang : Lang_intf.S_with_features with type Op.t = Cad_op.t and type Value.t = Cad_conc.t) = struct
  open Lang
  module Search_state = Search_state_append.Make (Lang)
  open Search_state

  let unsafe_to_list a = List.init (Option_array.length a) ~f:(Option_array.unsafe_get_some_assuming_some a)

  let make_edge params op args = (Value.eval params op args, op, args)

  let generate_args params ss op costs =
    let arity = Op.arity op in
    let types_ = Op.args_type op |> Array.of_list in
    let args = Option_array.create ~len:arity in
    let rec build_args arg_idx =
      if arg_idx >= arity then [ make_edge params op @@ unsafe_to_list args ]
      else
        search ss ~cost:costs.(arg_idx) ~type_:types_.(arg_idx)
        |> List.concat_map ~f:(fun v ->
               Option_array.set_some args arg_idx v;
               build_args (arg_idx + 1))
    in
    build_args 0

  let generate_states params ss ops cost =
    if cost = 1 then List.filter ops ~f:(fun op -> Op.arity op = 0) |> List.map ~f:(fun op -> make_edge params op [])
    else if cost > 1 then
      let arg_cost = cost - 1 in
      List.filter ops ~f:(fun op -> Op.arity op > 0)
      |> List.concat_map ~f:(fun op ->
             Combinat.compositions ~n:arg_cost ~k:(Op.arity op)
             |> Combinat.to_list
             |> List.concat_map ~f:(generate_args params ss op))
    else []

  exception Done of Op.t Program.t

  let dedup_states ss states =
    states
    |> List.filter ~f:(fun (s, _, _) -> not (mem ss s))
    |> List.dedup_and_sort ~compare:(fun (s, _, _) (s', _, _) -> [%compare: Value.t] s s')

  let sample_diverse params ss new_states =
    let states =
      Dumb_progress.List.map ~name:"sampling" new_states ~f:(fun ((_, new_state, _, _) as x) ->
          let min_dist =
            List.map (states ss) ~f:(fun old_state -> Value.dist params old_state new_state)
            |> List.min_elt ~compare:[%compare: float]
            |> Option.value ~default:Float.infinity
          in
          (min_dist, x))
      |> List.sort ~compare:(fun (d, _) (d', _) -> -[%compare: float] d d')
    in
    Sample.weighted_random (Params.get params per_cost) states ~weight:(fun (w, _) -> w) |> List.map ~f:Tuple.T2.get2

  let sample_naive params new_states =
    let states = List.permute new_states in
    List.take states (Params.get params per_cost)

  let sample_states params ss = if Params.get params diversity then sample_diverse params ss else sample_naive params

  let insert_states params ss cost states =
    List.iter states ~f:(fun (_, state, op, args) -> insert ss cost state op args);
    Params.get params bank_size := Float.of_int @@ Search_state.length ss

  let search_stochastic ?(sample_k = 15) params ops center output f =
    let open Sample.Incremental in
    let sample = reservoir_unique (module Value) sample_k in
    try
      Tree_ball.Rename_only.ball
        (module Op)
        ops center (Params.get params ball_width)
        (fun p ->
          let v = Program.eval (Value.eval params) p in
          let is_trivial = Cad_gen.non_trivial params p in
          Fmt.epr "Program:\n%a\nNon-trivial: %b\n%a\n\n" (Program.pp Cad_op.pp) p is_trivial Cad_conc.pprint v;
          sample.add v;
          if [%compare.equal: Value.t] v output then f p);
      sample.get_sample ()
    with Program.Eval_error e -> raise @@ Program.Eval_error [%message (center : Op.t Program.t) (e : Sexp.t)]

  let search_neighbors = search_stochastic

  let make_nd_classes classes =
    let open Owl in
    let classes = List.map ~f:(List.map ~f:Lang.features) classes in
    let n_features = (Arr.shape @@ List.hd_exn @@ List.hd_exn classes).(0) in
    let n_samples = List.sum (module Int) classes ~f:List.length in

    let samples_nd = Arr.create [| n_samples; n_features |] 0.0 in
    let classes_nd = Arr.create [| n_samples |] 0.0 in

    let sample_id = ref 0 in
    List.iteri classes ~f:(fun class_id elems ->
        List.iter elems ~f:(fun feat ->
            Arr.(samples_nd.${[ !sample_id ]; []} <- Arr.expand feat 2);
            Arr.(classes_nd.%{!sample_id} <- Float.of_int class_id);
            incr sample_id));
    (samples_nd, classes_nd)

  let temp_feature_file = "features.npy" (* Filename.temp_file "features" "npy" *)

  let temp_class_file = "classes.npy" (* Filename.temp_file "classes" "npy" *)

  let temp_distance_file = "distance.npy" (* Filename.temp_file "distance" "npy" *)

  let write_features classes =
    let features_nd, classes_nd = make_nd_classes classes in
    Npy.write features_nd temp_feature_file;
    Npy.write classes_nd temp_class_file

  let learn_distance classes =
    if List.length classes <= 1 then Ok ()
    else
      let features_nd, classes_nd = make_nd_classes classes in
      Npy.write features_nd temp_feature_file;
      Npy.write classes_nd temp_class_file;
      Unix.system [%string "bin/learn_distance.py %{temp_feature_file} %{temp_class_file} > %{temp_distance_file}"]
      |> Unix.Exit_or_signal.or_error

  let synth params =
    let max_cost = Params.get params max_cost in
    let ss = Search_state.create max_cost in
    let bench = Params.get params bench in
    let ops = Bench.ops bench and output = Bench.output bench in

    let thresh = Params.get params thresh
    and final_value_dist = Params.get params final_value_dist
    and final_program_dist = Params.get params final_program_dist
    and program_cost = Params.get params program_cost
    and found_program = Params.get params found_program in
    let search_neighbors = search_neighbors params in

    let classes = ref [ [ output; output; output ] ] in

    (try
       for cost = 0 to max_cost do
         let new_states = generate_states params ss ops cost |> dedup_states ss in

         let new_states = List.map new_states ~f:(fun (s, op, args) -> (Value.dist params output s, s, op, args)) in

         (* Check neighbors around new states *)
         List.iter new_states ~f:(fun (d, center_state, op, args) ->
             if Float.(d <= thresh) then (
               eprintf "local searching (total=%d)\n" (List.length new_states);
               let center = program_of_op_args_exn ss op args in

               let neighbor_sample =
                 search_neighbors ops center output (fun p ->
                     final_value_dist := d;
                     final_program_dist := Float.of_int @@ Tree_dist.zhang_sasha ~eq:[%compare.equal: Op.t] center p;
                     program_cost := Float.of_int cost;
                     found_program := true;
                     raise (Done p))
               in

               classes := (center_state :: neighbor_sample) :: !classes));

         let new_states = sample_states params ss new_states in
         insert_states params ss cost new_states;

         Fmt.epr "Finished cost %d\n%!" cost;
         print_stats ss
       done
     with Done p ->
       assert (Value.equal (Program.eval (Value.eval params) p) output);
       eprint_s [%message (p : Op.t Program.t)]);

    write_features !classes
end
