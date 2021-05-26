open Params

include struct
  open Dumb_params

  let spec = Spec.create ()

  let n_states = Spec.add spec @@ Param.float_ref ~name:"n_states" ()

  let n_iters = Spec.add spec @@ Param.float_ref ~name:"n_iters" ()

  let best_dist = Spec.add spec @@ Param.float_ref ~name:"best_dist" ()

  let found_program = Spec.add spec @@ Param.bool_ref ~name:"found_program" ()

  let synth = Spec.add spec @@ Param.const_str ~name:"synth" "sampling-naive"
end

module Make
    (Lang : Lang_intf.S
              with type Value.t = Cad.Value.t
               and type bench = Cad.Bench.t) =
struct
  open Lang

  module State_set = struct
    type t = Set.M(Value).t ref

    let create () = ref (Set.empty (module Value))

    let length x = Set.length !x

    let add s x = s := Set.add !s x

    let to_list x = Set.to_list !x

    let random_element_exn x =
      Option.value_exn (Set.nth !x (Random.int @@ length x))
  end

  let eval = Value.eval

  exception Done

  let hamming = Cad.Value.hamming

  let synth params =
    let bench = get params bench in
    let ops = Bench.ops bench
    and output = Bench.output bench
    and states = State_set.create ()
    and n_iters = get params n_iters
    and best_dist = get params best_dist
    and found_program = get params found_program
    and n_states = get params n_states in

    let non_nil_ops =
      List.filter ops ~f:(fun op -> Op.arity op > 0)
      |> List.sort ~compare:(fun o o' -> compare (Op.arity o) (Op.arity o'))
    in

    let fill () =
      let best = ref None in
      n_iters := 0.0;
      while true do
        n_iters := !n_iters +. 1.0;

        let op = List.random_element_exn non_nil_ops in
        let args =
          List.init (Op.arity op) ~f:(fun _ ->
              State_set.random_element_exn states)
        in
        let out = eval params op args in

        (match !best with
        | None ->
            best_dist := Float.of_int @@ hamming output out;
            best := Some out
        | Some _ ->
            let dist = Float.of_int @@ hamming output out in
            if Float.(dist < !best_dist) then (
              best_dist := dist;
              best := Some out));

        if Float.to_int !n_iters mod 10000 = 0 then
          eprint_s
            [%message (!best_dist : float) (State_set.length states : int)];

        if [%compare.equal: Value.t] out output then (
          found_program := true;
          raise Done);

        State_set.add states out;
        n_states := Float.of_int @@ State_set.length states
      done
    in

    try
      List.iter ops ~f:(fun op ->
          if Op.arity op = 0 then
            let state = eval params op [] in
            State_set.add states state);

      fill ()
    with Done -> ()
end
