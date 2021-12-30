module type Lang_intf = sig
  module Type : sig
    type t [@@deriving compare, hash, sexp]
  end

  module Op : sig
    type t [@@deriving compare, hash, sexp]

    val cost : t -> int
    val arity : t -> int
    val args_type : t -> Type.t list
    val is_commutative : t -> bool
  end

  module Value : sig
    type t [@@deriving compare, hash, sexp]

    module Ctx : sig
      type t
    end

    val eval : Ctx.t -> Op.t -> t list -> t
    val is_error : t -> bool
  end
end

module Gen_list (Lang : Lang_intf) = struct
  open Lang

  let unsafe_to_list a =
    List.init (Option_array.length a) ~f:(Option_array.unsafe_get_some_assuming_some a)

  let make_edge params op args =
    let state = Value.eval params op args in
    if Value.is_error state then [] else [ (state, op, args) ]

  let generate_args search params ss op costs =
    let arity = Op.arity op in
    let types_ = Op.args_type op |> Array.of_list in
    let args = Option_array.create ~len:arity in
    let rec build_args arg_idx =
      if arg_idx >= arity then make_edge params op @@ unsafe_to_list args
      else
        search ss ~cost:costs.(arg_idx) ~type_:types_.(arg_idx)
        |> List.concat_map ~f:(fun v ->
               Option_array.set_some args arg_idx v;
               build_args (arg_idx + 1))
    in
    build_args 0

  let generate_states search params ss ops cost =
    if cost <= 0 then []
    else
      List.concat_map ops ~f:(fun op ->
          let op_cost = Op.cost op and arity = Op.arity op in
          let args_cost = cost - op_cost in
          if args_cost < arity then []
          else if arity = 0 && args_cost = 0 then make_edge params op []
          else
            Combinat.compositions ~n:args_cost ~k:arity
            |> Combinat.to_list
            |> List.concat_map ~f:(generate_args search params ss op))
end

module Gen_iter (Lang : Lang_intf) = struct
  open Lang

  let unsafe_to_list a =
    List.init (Option_array.length a) ~f:(Option_array.unsafe_get_some_assuming_some a)

  let make_edge params op args = (Value.eval params op args, op, args)
  let max_arity = 10
  let args = Array.init max_arity ~f:(fun i -> Option_array.create ~len:i)

  let generate_args_1 search params ss op c t f =
    search ss ~cost:c ~type_:t (fun v ->
        let args = [ v ] in
        let state = Value.eval params op [ v ] in
        if not (Value.is_error state) then f (state, op, args))

  let generate_args_2 search params ss op c1 t1 c2 t2 f =
    search ss ~cost:c1 ~type_:t1 (fun v ->
        search ss ~cost:c2 ~type_:t2 (fun v' ->
            let args = [ v; v' ] in
            let state = Value.eval params op args in
            if not (Value.is_error state) then f (state, op, args)))

  let generate_args_n search params ss op costs types_ f =
    let arity = Op.arity op in
    let args = args.(arity) in
    let rec build_args arg_idx =
      if arg_idx >= arity then f @@ make_edge params op @@ unsafe_to_list args
      else
        search ss ~cost:costs.(arg_idx) ~type_:types_.(arg_idx)
        |> Iter.iter (fun v ->
               Option_array.set_some args arg_idx v;
               build_args (arg_idx + 1))
    in
    build_args 0

  let generate_args search params ss op costs f =
    match (costs, Op.args_type op) with
    | [| c |], [ t ] -> generate_args_1 search params ss op c t f
    | [| c1; c2 |], [ t1; t2 ] -> generate_args_2 search params ss op c1 t1 c2 t2 f
    | c, t -> generate_args_n search params ss op c (Array.of_list t) f

  let generate_states search params ss ops cost f =
    let op_iter = Iter.of_list ops in
    if cost = 1 then
      op_iter
      |> Iter.filter (fun op -> Op.arity op = 0)
      |> Iter.iter (fun op -> f @@ make_edge params op [])
    else if cost > 1 then
      let arg_cost = cost - 1 in
      op_iter
      |> Iter.filter (fun op ->
             let arity = Op.arity op in
             arity > 0 && arity < cost)
      |> Iter.iter (fun op ->
             let costs =
               if Op.is_commutative op then
                 Combinat.partitions ~n:arg_cost ~k:(Op.arity op)
               else Combinat.compositions ~n:arg_cost ~k:(Op.arity op)
             in
             costs (fun costs -> generate_args search params ss op costs f))
end

module Gen_queue (Lang : Lang_intf) = struct
  module Iter = Gen_iter (Lang)

  let generate_states search params ss ops cost queue =
    Iter.generate_states search params ss ops cost (Queue.enqueue queue)
end
