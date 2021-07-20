module Generate_list (Lang : Lang_intf.S) = struct
  open Lang

  let unsafe_to_list a = List.init (Option_array.length a) ~f:(Option_array.unsafe_get_some_assuming_some a)

  let make_edge params op args = (Value.eval params op args, op, args)

  let generate_args search params ss op costs =
    let arity = Op.arity op in
    let types_ = Op.args_type op |> Array.of_list in
    let args = Option_array.create ~len:arity in
    let rec build_args arg_idx =
      if arg_idx >= arity then [ make_edge params op @@ unsafe_to_list args ]
      else
        search ss ~cost:costs.(arg_idx) ~type_:types_.(arg_idx)
        |> List.concat_map ~f:(fun v ->
               Option_array.set_some args arg_idx v;
               build_args (arg_idx + 1))
    in
    build_args 0

  let generate_states search params ss ops cost =
    if cost = 1 then List.filter ops ~f:(fun op -> Op.arity op = 0) |> List.map ~f:(fun op -> make_edge params op [])
    else if cost > 1 then
      let arg_cost = cost - 1 in
      List.filter ops ~f:(fun op -> Op.arity op > 0)
      |> List.concat_map ~f:(fun op ->
             Combinat.compositions ~n:arg_cost ~k:(Op.arity op)
             |> Combinat.to_list
             |> List.concat_map ~f:(generate_args search params ss op))
    else []
end

module Generate_iter (Lang : Lang_intf.S) = struct
  open Lang

  let unsafe_to_list a = List.init (Option_array.length a) ~f:(Option_array.unsafe_get_some_assuming_some a)

  let make_edge params op args = (Value.eval params op args, op, args)

  let generate_args search params ss op costs f =
    let arity = Op.arity op in
    let types_ = Op.args_type op |> Array.of_list in
    let args = Option_array.create ~len:arity in
    let rec build_args arg_idx =
      if arg_idx >= arity then f @@ make_edge params op @@ unsafe_to_list args
      else
        search ss ~cost:costs.(arg_idx) ~type_:types_.(arg_idx)
        |> List.iter ~f:(fun v ->
               Option_array.set_some args arg_idx v;
               build_args (arg_idx + 1))
    in
    build_args 0

  let generate_states search params ss ops cost f =
    if cost = 1 then
      List.filter ops ~f:(fun op -> Op.arity op = 0) |> List.iter ~f:(fun op -> f @@ make_edge params op [])
    else if cost > 1 then
      let arg_cost = cost - 1 in
      List.filter ops ~f:(fun op -> Op.arity op > 0)
      |> List.iter ~f:(fun op ->
             Combinat.compositions ~n:arg_cost ~k:(Op.arity op) (fun costs -> generate_args search params ss op costs f))
end

module Generate_queue (Lang : Lang_intf.S) = struct
  module Iter = Generate_iter (Lang)

  let generate_states search params ss ops cost queue =
    Iter.generate_states search params ss ops cost (Queue.enqueue queue)
end
