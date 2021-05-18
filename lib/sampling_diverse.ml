module Make
    (Lang : Lang_intf.S)
    (Dist : Dist_intf.S with type value := Lang.Value.t and type op := Lang.Op.t) =
struct
  open Lang
  module Search_state = Search_state_append.Make (Lang)
  open Search_state

  type stats = {
    per_cost : int;
    thresh : float;
    ball_width : int;
    mutable bank_size : float;
    mutable value_dist : float;
    mutable program_dist : float;
    mutable program_cost : int;
    mutable found_program : bool;
  }

  let create ?(per_cost = 100) ?(thresh = 150.0) ?(ball_width = 2) () =
    {
      per_cost;
      thresh;
      ball_width;
      bank_size = Float.nan;
      value_dist = Float.nan;
      program_dist = Float.nan;
      program_cost = -1;
      found_program = false;
    }

  let unsafe_to_list a =
    List.init (Option_array.length a)
      ~f:(Option_array.unsafe_get_some_assuming_some a)

  let make_edge ss op args = (Value.eval (params ss) op args, op, args)

  let generate_args ss op costs =
    let arity = Op.arity op in
    let args = Option_array.create ~len:arity in
    let rec build_args arg_idx =
      if arg_idx >= arity then [ make_edge ss op @@ unsafe_to_list args ]
      else
        of_cost ss costs.(arg_idx)
        |> List.concat_map ~f:(fun v ->
               Option_array.set_some args arg_idx v;
               build_args (arg_idx + 1))
    in
    build_args 0

  let generate_states ss ops cost =
    if cost = 1 then
      List.filter ops ~f:(fun op -> Op.arity op = 0)
      |> List.map ~f:(fun op -> make_edge ss op [])
    else if cost > 1 then
      let arg_cost = cost - 1 in
      List.filter ops ~f:(fun op -> Op.arity op > 0)
      |> List.concat_map ~f:(fun op ->
             Combinat.compositions ~n:arg_cost ~k:(Op.arity op)
             |> Combinat.to_list
             |> List.concat_map ~f:(generate_args ss op))
    else []

  exception Done of Op.t Program.t

  let dedup_states ss states =
    states
    |> List.filter ~f:(fun (s, _, _) -> not (mem ss s))
    |> List.dedup_and_sort ~compare:(fun (s, _, _) (s', _, _) ->
           [%compare: Value.t] s s')

  let sample_states stats ss new_states =
    let states =
      Dumb_progress.List.map ~name:"sampling" new_states
        ~f:(fun ((new_state, _, _) as x) ->
          let min_dist =
            List.map (states ss) ~f:(fun old_state ->
                Dist.value old_state new_state)
            |> List.min_elt ~compare:[%compare: float]
            |> Option.value ~default:Float.infinity
          in
          (min_dist, x))
      |> List.sort ~compare:(fun (d, _) (d', _) -> -[%compare: float] d d')
    in
    Fmt.epr "Max %f Min %f\n%!"
      (List.hd states
      |> Option.map ~f:Tuple.T2.get1
      |> Option.value ~default:Float.nan)
      (List.last states
      |> Option.map ~f:Tuple.T2.get1
      |> Option.value ~default:Float.nan);
    Sample.weighted_random ~state:(params ss).random_state stats.per_cost states
      ~weight:(fun (w, _) -> w)

  let insert_states stats ss cost states =
    List.iter states ~f:(fun (state, op, args) -> insert ss cost state op args);
    stats.bank_size <- Float.of_int @@ Search_state.length ss

  let synth stats params =
    let ss = Search_state.create params
    and ops = Bench.ops params.bench
    and output = Bench.output params.bench in

    let eval =
      let module P = Program.Make (Op) in
      P.eval_memoized (Value.eval params)
    in
    let eval p =
      try eval p
      with Program.Eval_error e ->
        raise @@ Program.Eval_error [%message (p : Op.t Program.t) (e : Sexp.t)]
    in

    try
      let cost = ref 0 in
      while !cost <= params.max_cost do
        let new_states = generate_states ss ops !cost |> dedup_states ss in
        let new_states = sample_states stats ss new_states in
        insert_states stats ss !cost @@ List.map ~f:Tuple.T2.get2 new_states;

        (* Check balls around new states *)
        List.iter new_states ~f:(fun (d, (s, _, _)) ->
            if Float.(d < stats.thresh) then
              let center = program_exn ss s in
              try
                Tree_ball.ball (module Op) ops center stats.ball_width
                @@ fun p ->
                if [%compare.equal: Value.t] (eval p) output then (
                  stats.value_dist <- d;
                  stats.program_dist <-
                    Float.of_int
                    @@ Tree_dist.zhang_sasha ~eq:[%compare.equal: Op.t] center p;
                  stats.program_cost <- !cost;
                  stats.found_program <- true;
                  raise (Done p))
              with Program.Eval_error e ->
                raise
                @@ Program.Eval_error
                     [%message (center : Op.t Program.t) (e : Sexp.t)]);

        Fmt.epr "Finished cost %d\n%!" !cost;
        print_stats ss;
        cost := !cost + 1
      done
    with Done p -> eprint_s [%message (p : Op.t Program.t)]
end
