include struct
  open Dumb_params

  let spec = Spec.create ()

  let synth = Spec.add spec @@ Param.const_str ~name:"synth" "baseline"

  let max_cost = Spec.add spec @@ Param.int ~name:"max-cost" ~doc:" max search cost" ()
end

module Make (Lang : Lang_intf.S) = struct
  open Lang
  module Search_state = Search_state_append.Make (Lang)
  open Search_state
  include Synth_utils.Generate_list (Lang)

  let generate_states = generate_states Search_state.search

  exception Done of Op.t Program.t

  let sample_states _ = Fun.id

  let insert_states ss cost states = List.iter states ~f:(fun (state, op, args) -> insert ss cost state op args)

  let fill fill params ss ops max_cost output cost =
    if cost > max_cost then ()
    else
      let new_states = generate_states params ss ops cost in
      let new_states = sample_states ss new_states in
      insert_states ss cost new_states;

      let solutions =
        List.filter_map new_states ~f:(fun (s, _, _) ->
            if [%compare.equal: Value.t] s output then Some (program_exn ss s) else None)
      in

      Fmt.epr "Finished cost %d\n%!" cost;
      print_stats ss;

      if not (List.is_empty solutions) then (
        List.iter solutions ~f:(fun p -> eprint_s [%message (p : Op.t Program.t)]);
        raise (Done (List.hd_exn solutions)));

      fill params ss ops max_cost output (cost + 1)

  let synth params =
    let max_cost = Params.get params max_cost in
    let ss = Search_state.create max_cost in
    let bench = Params.get params Lang.bench in
    let ops = Bench.ops bench and output = Bench.output bench in
    let rec fill_ params = fill fill_ params in
    try fill_ params ss ops max_cost output 0 with Done p -> eprint_s [%message (p : Op.t Program.t)]
end
