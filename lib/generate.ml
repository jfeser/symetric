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
            |> Iter.to_list
            |> List.concat_map ~f:(generate_args search params ss op))
end

module Gen_iter (Lang : Lang_intf) = struct
  open Lang

  let unsafe_to_list a =
    List.init (Option_array.length a) ~f:(Option_array.unsafe_get_some_assuming_some a)

  let[@inline] make_edge to_value params op args =
    (Value.eval params op @@ List.map ~f:to_value args, op, args)

  let generate_args_1 search to_value params ss op c t f =
    search ss ~cost:c ~type_:t (fun v ->
        let args = [ v ] in
        let state = Value.eval params op @@ List.map ~f:to_value args in
        if not (Value.is_error state) then f (state, op, args))

  let generate_args_2 search to_value params ss op c1 t1 c2 t2 f =
    if Op.is_commutative op then
      search ss ~cost:c1 ~type_:t1
      |> Iter.iteri (fun i v ->
             search ss ~cost:c2 ~type_:t2 |> Iter.drop i
             |> Iter.iter (fun v' ->
                    let args = [ v; v' ] in
                    let state = Value.eval params op @@ List.map ~f:to_value args in
                    if not (Value.is_error state) then f (state, op, args)))
    else
      search ss ~cost:c1 ~type_:t1
      |> Iter.iter (fun v ->
             search ss ~cost:c2 ~type_:t2
             |> Iter.iter (fun v' ->
                    let args = [ v; v' ] in
                    let state = Value.eval params op @@ List.map ~f:to_value args in
                    if not (Value.is_error state) then f (state, op, args)))

  let generate_args_n search to_value params ss op costs types_ f =
    let rec build_args i k =
      let cost = costs.(i) in
      function
      | [] -> k []
      | [ t ] -> search ss ~cost ~type_:t (fun v -> k [ v ])
      | t :: ts ->
          build_args (i + 1)
            (fun vs -> search ss ~cost ~type_:t (fun v -> k (v :: vs)))
            ts
    in
    build_args 0
      (fun args ->
        let state = Value.eval params op @@ List.map ~f:to_value args in
        if not (Value.is_error state) then f (state, op, args))
      types_

  let generate_args search to_value params ss op costs f =
    match (costs, Op.args_type op) with
    | [| c |], [ t ] -> generate_args_1 search to_value params ss op c t f
    | [| c1; c2 |], [ t1; t2 ] ->
        generate_args_2 search to_value params ss op c1 t1 c2 t2 f
    | c, t -> generate_args_n search to_value params ss op c t f

  let generate_states search to_value params ss ops cost f =
    let op_iter = Iter.of_list ops in
    if cost = 1 then
      op_iter
      |> Iter.filter (fun op -> Op.arity op = 0)
      |> Iter.iter (fun op -> f @@ make_edge to_value params op [])
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
             costs (fun costs -> generate_args search to_value params ss op costs f))
end

let%expect_test "" =
  let module L = struct
    module Type = struct
      type t = Odd | Even [@@deriving compare, hash, sexp]
    end

    module Op = struct
      type t = Plus | Minus [@@deriving compare, hash, sexp]

      let cost _ = 1
      let arity _ = 2

      let args_type : _ -> Type.t list = function
        | Plus -> [ Even; Odd ]
        | Minus -> [ Odd; Even ]

      let is_commutative _ = false
    end

    module Value = struct
      type t = int [@@deriving compare, hash, sexp]

      module Ctx = struct
        type t = unit
      end

      let eval _ (op : Op.t) args =
        match (op, args) with
        | Plus, [ a; b ] -> a + b
        | Minus, [ a; b ] -> a - b
        | _ -> assert false

      let is_error _ = false
    end
  end in
  let open L in
  let module G = Gen_iter (L) in
  let search _ ~cost ~type_ =
    match (cost, (type_ : Type.t)) with
    | 1, Even -> Iter.singleton 0
    | 1, Odd -> Iter.singleton 1
    | _ -> assert false
  in
  let to_value = Fun.id in
  G.generate_states search to_value () () [ Plus; Minus ] 3 (fun (value, op, args) ->
      print_s [%message (value : Value.t) (op : Op.t) (args : Value.t list)]);
  [%expect
    {|
    ((value 1) (op Plus) (args (0 1)))
    ((value 1) (op Minus) (args (1 0))) |}]
