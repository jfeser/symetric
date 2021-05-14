module Make
    (Lang : Lang_intf.S)
    (Dist : Dist_intf.S
              with type value := Lang.Value.t
               and type params := Lang.params
               and type op := Lang.Op.t) =
struct
  open Lang
  module Search_state = Search_state_append.Make (Lang)
  open Search_state

  let k = 100

  and thresh = 150.0

  and ball_width = 2

  let unsafe_to_list a =
    List.init (Option_array.length a)
      ~f:(Option_array.unsafe_get_some_assuming_some a)

  let eval ss p =
    try Program.eval (Value.eval (params ss)) p
    with Program.Eval_error e ->
      raise @@ Program.Eval_error [%message (p : Op.t Program.t) (e : Sexp.t)]

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
    List.dedup_and_sort
      ~compare:(fun (s, _, _) (s', _, _) -> [%compare: Value.t] s s')
      states
    |> List.filter ~f:(fun (s, _, _) -> not (mem ss s))

  let sample_states ss new_states =
    let states =
      Dumb_progress.List.map ~name:"sampling" new_states
        ~f:(fun ((new_state, _, _) as x) ->
          let min_dist =
            List.map (states ss) ~f:(fun old_state ->
                Dist.value (params ss) old_state new_state)
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
    states |> fun l -> List.take l k

  let insert_states ss cost states =
    List.iter states ~f:(fun (state, op, args) -> insert ss cost state op args)

  let synth params =
    let ss = Search_state.create params
    and ops = Bench.ops params.bench
    and output = Bench.output params.bench in

    try
      let cost = ref 0 in
      while !cost <= params.max_cost do
        let new_states = generate_states ss ops !cost |> dedup_states ss in
        let new_states = sample_states ss new_states in
        insert_states ss !cost @@ List.map ~f:Tuple.T2.get2 new_states;

        (* Check balls around new states *)
        List.iter new_states ~f:(fun (d, (s, _, _)) ->
            if Float.(d < thresh) then
              let center = program_exn ss s in
              try
                Tree_ball.ball (module Op) ops center ball_width @@ fun p ->
                if [%compare.equal: Value.t] (eval ss p) output then
                  raise (Done p)
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
