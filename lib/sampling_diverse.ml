include struct
  open Dumb_params

  let float_list ~name ?(csv = true) () =
    (module struct
      type t = float list Queue.t [@@deriving sexp_of]

      let key = Univ_map.Key.create ~name [%sexp_of: t]

      let name = name

      let init = Second (fun () -> Queue.create ())

      let to_json =
        if csv then
          Option.return @@ fun v ->
          `List
            (Queue.to_list v
            |> List.map ~f:(fun l -> `List (List.map l ~f:(fun x -> `Float x)))
            )
        else None
    end : Param.S
      with type t = float list Queue.t)

  let spec = Spec.create ()

  let per_cost =
    Spec.add spec
    @@ Param.int ~name:"per-cost" ~aliases:[ "p" ]
         ~doc:" programs stored per cost level" ~init:(`Cli (Some 100)) ()

  let thresh =
    Spec.add spec
    @@ Param.float ~name:"thresh" ~aliases:[ "d" ]
         ~doc:" exhaustive search threshold" ~init:(`Cli (Some 1.0)) ()

  let ball_width =
    Spec.add spec
    @@ Param.int ~name:"width" ~aliases:[ "w" ] ~doc:" exhaustive search width"
         ~init:(`Cli (Some 2)) ()

  let diversity =
    Spec.add spec
    @@ Param.bool ~name:"diversity" ~doc:" use diversity sampling"
         ~init:(`Cli (Some true)) ()

  let bank_size = Spec.add spec @@ Param.float_ref ~name:"bank-size" ()

  let final_value_dist =
    Spec.add spec @@ Param.float_ref ~name:"final-value-dist" ()

  let final_program_dist =
    Spec.add spec @@ Param.float_ref ~name:"final-program-dist" ()

  let program_cost = Spec.add spec @@ Param.float_ref ~name:"program-cost" ()

  let found_program = Spec.add spec @@ Param.bool_ref ~name:"found-program" ()

  let closest_program =
    Spec.add spec @@ Param.float_ref ~name:"closest-program" ()

  let have_parts = Spec.add spec @@ Param.float_ref ~name:"have-parts" ()

  let total_parts = Spec.add spec @@ Param.float_ref ~name:"total-parts" ()

  let synth = Spec.add spec @@ Param.const_str ~name:"synth" "sampling-diverse"

  let value_dist =
    Spec.add spec @@ Param.create @@ float_list ~name:"value-dist" ()

  (* let program_ball_dist =
   *   Spec.add spec @@ Param.create @@ float_list ~name:"program-ball-dist" () *)

  let program_zs_dist =
    Spec.add spec @@ Param.create @@ float_list ~name:"program-zs-dist" ()
end

module Make
    (Lang : Lang_intf.S with type Value.t = Cad_conc.t)
    (Dist : Dist_intf.S with type value := Lang.Value.t and type op := Lang.Op.t) =
struct
  open Lang
  module Search_state = Search_state_append.Make (Lang)
  open Search_state

  let unsafe_to_list a =
    List.init (Option_array.length a)
      ~f:(Option_array.unsafe_get_some_assuming_some a)

  let make_edge params op args = (Value.eval params op args, op, args)

  let generate_args params ss op costs =
    let arity = Op.arity op in
    let args = Option_array.create ~len:arity in
    let rec build_args arg_idx =
      if arg_idx >= arity then [ make_edge params op @@ unsafe_to_list args ]
      else
        of_cost ss costs.(arg_idx)
        |> List.concat_map ~f:(fun v ->
               Option_array.set_some args arg_idx v;
               build_args (arg_idx + 1))
    in
    build_args 0

  let generate_states params ss ops cost =
    if cost = 1 then
      List.filter ops ~f:(fun op -> Op.arity op = 0)
      |> List.map ~f:(fun op -> make_edge params op [])
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
    |> List.dedup_and_sort ~compare:(fun (s, _, _) (s', _, _) ->
           [%compare: Value.t] s s')

  let sample_diverse params ss new_states =
    let states =
      Dumb_progress.List.map ~name:"sampling" new_states
        ~f:(fun ((_, new_state, _, _) as x) ->
          let min_dist =
            List.map (states ss) ~f:(fun old_state ->
                Dist.value old_state new_state)
            |> List.min_elt ~compare:[%compare: float]
            |> Option.value ~default:Float.infinity
          in
          (min_dist, x))
      |> List.sort ~compare:(fun (d, _) (d', _) -> -[%compare: float] d d')
    in
    Sample.weighted_random (Params.get params per_cost) states
      ~weight:(fun (w, _) -> w)
    |> List.map ~f:Tuple.T2.get2

  let sample_naive params new_states =
    let states = List.permute new_states in
    List.take states (Params.get params per_cost)

  let sample_states params ss =
    if Params.get params diversity then sample_diverse params ss
    else sample_naive params

  let insert_states params ss cost states =
    List.iter states ~f:(fun (_, state, op, args) ->
        insert ss cost state op args);
    Params.get params bank_size := Float.of_int @@ Search_state.length ss

  let synth params =
    let max_cost = Params.get params Params.max_cost in
    let ss = Search_state.create max_cost in
    let bench = Params.get params bench in
    let ops = Bench.ops bench
    and output = Bench.output bench
    and solution = Bench.solution_exn bench in

    let thresh = Params.get params thresh
    and ball_width = Params.get params ball_width
    and final_value_dist = Params.get params final_value_dist
    and final_program_dist = Params.get params final_program_dist
    and program_cost = Params.get params program_cost
    and found_program = Params.get params found_program
    and have_parts = Params.get params have_parts
    and total_parts = Params.get params total_parts
    and value_dist = Params.get params value_dist
    (* and program_ball_dist = Params.get params program_ball_dist *)
    and program_zs_dist = Params.get params program_zs_dist in

    let solution_parts =
      Program.eval_parts (Value.eval params) solution
      |> List.dedup_and_sort ~compare:[%compare: Value.t]
    in
    total_parts := Float.of_int @@ List.length solution_parts;

    let eval = Program.eval (Value.eval params) in
    let cost = ref 0 in
    (try
       while !cost <= max_cost do
         let new_states =
           generate_states params ss ops !cost |> dedup_states ss
         in

         let new_states =
           List.map new_states ~f:(fun (s, op, args) ->
               (Dist.value output s, s, op, args))
         in

         Queue.enqueue value_dist
         @@ List.map new_states ~f:(fun (d, _, _, _) -> d);

         (* Queue.enqueue program_ball_dist
          * @@ List.map new_states ~f:(fun (_, _, op, args) ->
          *        let p = program_of_op_args_exn ss op args in
          *        Tree_ball.dist ~compare:[%compare: Op.t] p solution); *)
         Queue.enqueue program_zs_dist
         @@ List.map new_states ~f:(fun (_, _, op, args) ->
                let p = program_of_op_args_exn ss op args in
                Float.of_int
                @@ Tree_dist.zhang_sasha ~eq:[%compare.equal: Op.t] p solution);

         (* Check balls around new states *)
         List.iter new_states ~f:(fun (d, _, op, args) ->
             if Float.(d < thresh) then
               let center = program_of_op_args_exn ss op args in

               (* let zd =
                *   Float.of_int
                *   @@ Tree_dist.zhang_sasha ~eq:[%compare.equal: Op.t] center
                *        solution
                * in
                * if Float.(zd < 6.0) then (
                *   Fmt.pr "%a\n" Cad_conc.pprint st;
                *   Fmt.pr "%a\n" Cad_conc.pprint output;
                * 
                *   Program.eval_parts (Value.eval params) solution
                *   |> List.dedup_and_sort ~compare:[%compare: Value.t]
                *   |> List.iteri ~f:(fun i p ->
                *          Fmt.pr "Sol part %d\n%a\n" i Cad_conc.pprint p);
                *   Program.eval_parts (Value.eval params) center
                *   |> List.dedup_and_sort ~compare:[%compare: Value.t]
                *   |> List.iteri ~f:(fun i p ->
                *          Fmt.pr "Can part %d\n%a\n" i Cad_conc.pprint p);
                * 
                *   print_s
                *     [%message
                *       (center : Op.t Program.t) (solution : Op.t Program.t)];
                *   Tree_dist.print_zhang_sasha_diff (module Op) center solution); *)
               try
                 Tree_ball.Rename_insert_delete.ball
                   (module Op)
                   ops center ball_width
                 @@ fun p ->
                 if [%compare.equal: Value.t] (eval p) output then (
                   final_value_dist := d;
                   final_program_dist :=
                     Float.of_int
                     @@ Tree_dist.zhang_sasha ~eq:[%compare.equal: Op.t] center
                          p;
                   program_cost := Float.of_int !cost;
                   found_program := true;
                   raise (Done p))
               with Program.Eval_error e ->
                 raise
                 @@ Program.Eval_error
                      [%message (center : Op.t Program.t) (e : Sexp.t)]);

         let new_states = sample_states params ss new_states in
         insert_states params ss !cost new_states;

         have_parts := Float.of_int @@ List.count solution_parts ~f:(mem ss);

         Fmt.epr "Finished cost %d\n%!" !cost;
         print_stats ss;
         cost := !cost + 1
       done
     with Done p -> eprint_s [%message (p : Op.t Program.t)]);

    Params.get params closest_program
    := List.map (states ss) ~f:(fun s ->
           let p = program_exn ss s in
           Dist.program solution p)
       |> List.min_elt ~compare:[%compare: float]
       |> Option.value_exn
end
