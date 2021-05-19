module P = Dumb_params

let per_cost =
  P.int ~name:"per-cost" ~aliases:[ "p" ] ~doc:" programs stored per cost level"
    ~init:(`Cli (Some 100)) ()

let thresh =
  P.float ~name:"thresh" ~aliases:[ "d" ] ~doc:" exhaustive search threshold"
    ~init:(`Cli (Some 1.0)) ()

let ball_width =
  P.int ~name:"width" ~aliases:[ "w" ] ~doc:" exhaustive search width"
    ~init:(`Cli (Some 2)) ()

let diversity =
  P.bool ~name:"diversity" ~doc:" use diversity sampling"
    ~init:(`Cli (Some true)) ()

let bank_size = P.float_ref ~name:"bank-size" ()

let value_dist = P.float_ref ~name:"value-dist" ()

let program_dist = P.float_ref ~name:"program-dist" ()

let program_cost = P.float_ref ~name:"program-cost" ()

let found_program = P.bool_ref ~name:"found-program" ()

let synth = P.const_str ~name:"synth" "sampling-diverse"

let spec =
  [
    P.to_spec per_cost;
    P.to_spec thresh;
    P.to_spec ball_width;
    P.to_spec diversity;
    P.to_spec bank_size;
    P.to_spec value_dist;
    P.to_spec program_dist;
    P.to_spec program_cost;
    P.to_spec found_program;
    P.to_spec synth;
  ]

module Make
    (Lang : Lang_intf.S)
    (Dist : Dist_intf.S with type value := Lang.Value.t and type op := Lang.Op.t) =
struct
  open Lang
  module Search_state = Search_state_append.Make (Lang)
  open Search_state

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

  let sample_diverse ss new_states =
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
    Sample.weighted_random
      (Params.get (params ss) per_cost)
      states
      ~weight:(fun (w, _) -> w)
    |> List.map ~f:Tuple.T2.get2

  let sample_naive ss new_states =
    let states = List.permute new_states in
    List.take states (Params.get (params ss) per_cost)

  let sample_states ss =
    if Params.get (params ss) diversity then sample_diverse ss
    else sample_naive ss

  let insert_states ss cost states =
    List.iter states ~f:(fun (state, op, args) -> insert ss cost state op args);
    Params.get (params ss) bank_size := Float.of_int @@ Search_state.length ss

  let synth params =
    let ss = Search_state.create params in
    let bench = Params.get params bench in
    let ops = Bench.ops bench and output = Bench.output bench in

    let thresh = Params.get params thresh
    and ball_width = Params.get params ball_width
    and max_cost = Params.get params Params.max_cost
    and value_dist = Params.get params value_dist
    and program_dist = Params.get params program_dist
    and program_cost = Params.get params program_cost
    and found_program = Params.get params found_program in

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
      while !cost <= max_cost do
        let new_states = generate_states ss ops !cost |> dedup_states ss in
        let new_states = sample_states ss new_states in
        insert_states ss !cost new_states;

        (* Check balls around new states *)
        List.iter new_states ~f:(fun (s, _, _) ->
            let d = Dist.value output s in
            if Float.(d < thresh) then
              let center = program_exn ss s in
              try
                Tree_ball.ball (module Op) ops center ball_width @@ fun p ->
                if [%compare.equal: Value.t] (eval p) output then (
                  value_dist := d;
                  program_dist :=
                    Float.of_int
                    @@ Tree_dist.zhang_sasha ~eq:[%compare.equal: Op.t] center p;
                  program_cost := Float.of_int !cost;
                  found_program := true;
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
