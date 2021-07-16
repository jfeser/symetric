open Params

include struct
  open Dumb_params

  let spec = Spec.create ()

  let n_states = Spec.add spec @@ Param.float_ref ~name:"n_states" ()

  let n_iters = Spec.add spec @@ Param.float_ref ~name:"n_iters" ()

  let best_dist = Spec.add spec @@ Param.float_ref ~name:"best_dist" ()

  let found_program = Spec.add spec @@ Param.bool_ref ~name:"found_program" ()

  let synth = Spec.add spec @@ Param.const_str ~name:"synth" "sampling-naive"

  let thresh =
    Spec.add spec
    @@ Param.float ~name:"thresh" ~aliases:[ "d" ] ~doc:" exhaustive search threshold" ~init:(`Cli (Some 1.0)) ()

  let ball_width =
    Spec.add spec @@ Param.int ~name:"width" ~aliases:[ "w" ] ~doc:" exhaustive search width" ~init:(`Cli (Some 2)) ()
end

module Make (Lang : Lang_intf.S with type Value.t = Cad.Value.t and type Bench.t = Cad.Bench.t) = struct
  open Lang
  module Search_state = Search_state_append.Make (Lang)

  exception Done

  let synth params =
    let bench = get params bench in
    let ops = Bench.ops bench
    and output = Bench.output bench
    and search_state = Search_state.create 100
    and states = ref []
    and n_iters = get params n_iters
    and found_program = get params found_program
    and n_states = get params n_states
    and thresh = get params thresh
    and ball_width = get params ball_width in

    let non_nil_ops =
      List.filter ops ~f:(fun op -> Op.arity op > 0)
      |> List.sort ~compare:(fun o o' -> compare (Op.arity o) (Op.arity o'))
    in

    let fill () =
      n_iters := 0.0;
      while true do
        n_iters := !n_iters +. 1.0;
        if Float.to_int !n_iters mod 100 = 0 then Fmt.epr "Iter %f...\n%!" !n_iters;

        let op = List.random_element_exn non_nil_ops in
        let args = List.init (Op.arity op) ~f:(fun _ -> List.random_element_exn !states) in
        let value = Value.eval params op args in
        if not @@ Search_state.mem search_state value then (
          Search_state.insert search_state 0 value op args;
          states := value :: !states;

          let dist = Value.dist params output value in
          if Float.(dist < thresh) then
            let center = Search_state.program_exn search_state value in
            Tree_ball.Rename_insert_delete.ball
              (module Op)
              ops center ball_width
              (fun p ->
                let v = Program.eval (Value.eval params) p in
                if [%compare.equal: Value.t] v output then (
                  found_program := true;
                  raise Done)));

        n_states := Float.of_int @@ List.length !states
      done
    in

    try
      List.iter ops ~f:(fun op ->
          if Op.arity op = 0 then (
            let state = Value.eval params op [] in
            Search_state.insert search_state 0 state op [];
            states := state :: !states));

      fill ()
    with Done -> ()
end
