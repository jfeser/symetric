module Make (Lang : Lang_intf.S) = struct
  open Lang
  module Search_state = Search_state.Make (Lang)
  open Search_state

  let unsafe_to_list a =
    List.init (Option_array.length a)
      ~f:(Option_array.unsafe_get_some_assuming_some a)

  let make_edge ss op args = (Conc.eval (params ss) op args, op, args)

  let generate_args ss op costs =
    let arity = Op.arity op in
    let arg_types = Op.args_type op |> Array.of_list in
    let args = Option_array.create ~len:arity in
    let rec build_args arg_idx =
      if arg_idx >= arity then make_edge ss op @@ unsafe_to_list args
      else
        state_set
          ~cost:(Combinat.Int_array.get costs arg_idx)
          ~type_:arg_types.(arg_idx)
        |> List.iter ~f:(fun v ->
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
             let module Comp = Combinat.Composition in
             Comp.create ~n:arg_cost ~k:(Op.arity op)
             |> Comp.to_list
             |> List.map ~f:(generate_args ss op))
    else []

  let dedup_states ss states =
    List.dedup_and_sort
      ~compare:(fun (s, _, _) (s', _, _) -> [%compare: Conc.t] s s')
      states
    |> List.filter ~f:(fun (state, _, _) -> not (Search_state.mem ss state))

  let[@landmark "fill"] fill_up_to_cost ss ops cost =
    let rec fill c =
      if c > cost then ()
      else
        let states = state_set ss in
        fill_cost ss states ops c;
        fill (c + 1)
    in
    fill 1

  exception Done

  let synth params =
    let ss = Search_state.create params
    and ops = Bench.ops params.bench
    and output = Bench.output params.bench in

    let rec fill cost =
      if cost > params.max_cost then ()
      else
        let new_states = generate_states ss ops cost |> dedup_states ss in

        if
          List.exists new_states ~f:(fun (s, _, _) ->
              [%compare.equal: Conc.t] s output)
        then raise Done;

        let new_states = sample_states ss new_states in
        insert_states ss new_states
        if List.is_empty targets then fill (cost + 1)
        else (
          refute ss targets;
          validate ss;
          fill cost)
    in

    (try fill 0 with Done -> ());
    (prog, ss)
end
