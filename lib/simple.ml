module Make (Lang : Lang_intf.S) = struct
  open Lang
  module Search_state = Search_state_append.Make (Lang)
  open Search_state

  let unsafe_to_list a =
    List.init (Option_array.length a)
      ~f:(Option_array.unsafe_get_some_assuming_some a)

  let make_edge ss cost op args =
    let state = Conc.eval (params ss) op args in
    insert ss cost state op args

  let generate_args ss op cost costs =
    let arity = Op.arity op in
    let args = Option_array.create ~len:arity in
    let rec build_args arg_idx =
      if arg_idx >= arity then make_edge ss cost op @@ unsafe_to_list args
      else
        of_cost ss (Combinat.Int_array.get costs arg_idx)
        |> List.iter ~f:(fun v ->
               Option_array.set_some args arg_idx v;
               build_args (arg_idx + 1))
    in
    build_args 0

  let generate_states ss ops cost =
    if cost = 1 then
      List.filter ops ~f:(fun op -> Op.arity op = 0)
      |> List.iter ~f:(fun op -> make_edge ss cost op [])
    else if cost > 1 then
      let arg_cost = cost - 1 in
      List.filter ops ~f:(fun op -> Op.arity op > 0)
      |> List.iter ~f:(fun op ->
             let module Comp = Combinat.Composition in
             Comp.create ~n:arg_cost ~k:(Op.arity op)
             |> Comp.iter ~f:(generate_args ss op cost))

  exception Done

  let sample_states _ = Fun.id

  let insert_states ss cost states =
    List.iter states ~f:(fun (state, op, args) -> insert ss cost state op args)

  let synth params =
    let ss = Search_state.create params
    and ops = Bench.ops params.bench
    and output = Bench.output params.bench in

    let rec fill cost =
      if cost > params.max_cost then () else generate_states ss ops cost;

      if mem ss output then raise Done;

      Fmt.epr "Finished cost %d\n%!" cost;
      print_stats ss;
      fill (cost + 1)
    in

    try fill 0 with Done -> Fmt.epr "Found solution"
end
