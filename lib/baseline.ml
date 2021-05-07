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
        of_cost ss (Combinat.Int_array.get costs arg_idx)
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
             let module Comp = Combinat.Composition in
             Comp.create ~n:arg_cost ~k:(Op.arity op)
             |> Comp.to_list
             |> List.concat_map ~f:(generate_args ss op))
    else []

  exception Done of Op.t Program.t

  let sample_states _ = Fun.id

  let insert_states ss cost states =
    List.iter states ~f:(fun (state, op, args) -> insert ss cost state op args)

  let synth params =
    let ss = Search_state.create params
    and ops = Bench.ops params.bench
    and output = Bench.output params.bench
    and solution = Bench.solution_exn params.bench in

    let rec fill cost =
      if cost > params.max_cost then ()
      else
        let new_states = generate_states ss ops cost in
        let new_states = sample_states ss new_states in
        insert_states ss cost new_states;

        List.iter new_states ~f:(fun (s, _, _) ->
            let p = program_exn ss s and p' = solution in
            if Program.size p = Program.size p' then
              let _d = Dist.value params s output in
              ()
            (* let td = Dist.program p p' in
             * Fmt.pr "%f,%f\n" d td *));

        let solutions =
          List.filter_map new_states ~f:(fun (s, _, _) ->
              if Float.(Dist.value params s output <= 0.0) then
                Some (program_exn ss s)
              else None)
        in
        if not (List.is_empty solutions) then (
          List.iter solutions ~f:(fun p ->
              eprint_s [%message (p : Op.t Program.t)]);
          raise (Done (List.hd_exn solutions)));

        Fmt.epr "Finished cost %d\n%!" cost;
        print_stats ss;
        fill (cost + 1)
    in

    try fill 0 with Done p -> eprint_s [%message (p : Op.t Program.t)]
end
