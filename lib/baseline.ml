include struct
  open Dumb_params

  let spec = Spec.create ()

  let synth = Spec.add spec @@ Param.const_str ~name:"synth" "baseline"

  let max_cost =
    Spec.add spec @@ Param.int ~name:"max-cost" ~doc:" max search cost" ()
end

module Make
    (Lang : Lang_intf.S)
    (Dist : Dist_intf.S with type value := Lang.Value.t and type op := Lang.Op.t)
    (Validate : sig
      open Lang

      val validate : (Value.t * Op.t * Value.t list) list -> unit
    end) =
struct
  open Lang
  module Search_state = Search_state_append.Make (Lang)
  open Search_state

  let dump_solution_dists = false

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

  let sample_states _ = Fun.id

  let insert_states ss cost states =
    List.iter states ~f:(fun (state, op, args) -> insert ss cost state op args)

  let synth params =
    let max_cost = Params.get params max_cost in
    let ss = Search_state.create max_cost in
    let bench = Params.get params Lang.bench in
    let ops = Bench.ops bench and output = Bench.output bench in

    let rec fill cost =
      if cost > max_cost then ()
      else
        let new_states = generate_states params ss ops cost in
        Validate.validate new_states;
        let new_states = sample_states ss new_states in
        insert_states ss cost new_states;

        let solutions =
          List.filter_map new_states ~f:(fun (s, _, _) ->
              if Float.(Dist.value s output <= 0.0) then Some (program_exn ss s)
              else None)
        in

        if dump_solution_dists && not (List.is_empty solutions) then
          List.iteri new_states ~f:(fun i (s, _, _) ->
              let p = program_exn ss s in
              let d = Dist.value s output in
              let td =
                List.map solutions ~f:(Dist.program p)
                |> List.min_elt ~compare:[%compare: float]
                |> Option.value ~default:Float.nan
              in
              eprintf "%d/%d\n%!" i (List.length new_states);
              Fmt.pr "%f,%f\n%!" d td);

        Fmt.epr "Finished cost %d\n%!" cost;
        print_stats ss;

        if not (List.is_empty solutions) then (
          List.iter solutions ~f:(fun p ->
              eprint_s [%message (p : Op.t Program.t)]);
          raise (Done (List.hd_exn solutions)));

        fill (cost + 1)
    in

    try fill 0 with Done p -> eprint_s [%message (p : Op.t Program.t)]
end
