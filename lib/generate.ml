module type DSL = sig
  module Type : sig
    type t
  end

  module Op : sig
    type t

    val arity : t -> int
    val args_type : t -> Type.t list
    val is_commutative : t -> bool
  end

  module Value : sig
    type t

    val eval : Op.t -> t list -> t
    val is_error : t -> bool
  end
end

let unsafe_to_list a =
  List.init (Option_array.length a) ~f:(Option_array.unsafe_get_some_assuming_some a)

let generate (type type_ value op)
    (module Dsl : DSL with type Type.t = type_ and type Value.t = value and type Op.t = op)
    search to_value ops cost =
  let open Dsl in
  let generate_args_1 op c t f =
    search ~cost:c ~type_:t (fun v ->
        let args = [ v ] in
        let state = Value.eval op @@ List.map ~f:to_value args in
        if not (Value.is_error state) then f (state, op, args))
  in
  let generate_args_2 op c1 t1 c2 t2 f =
    if Op.is_commutative op then
      search ~cost:c1 ~type_:t1
      |> Iter.iteri (fun i v ->
             search ~cost:c2 ~type_:t2 |> Iter.drop i
             |> Iter.iter (fun v' ->
                    let args = [ v; v' ] in
                    let state = Value.eval op @@ List.map ~f:to_value args in
                    if not (Value.is_error state) then f (state, op, args)))
    else
      search ~cost:c1 ~type_:t1
      |> Iter.iter (fun v ->
             search ~cost:c2 ~type_:t2
             |> Iter.iter (fun v' ->
                    let args = [ v; v' ] in
                    let state = Value.eval op @@ List.map ~f:to_value args in
                    if not (Value.is_error state) then f (state, op, args)))
  in
  let generate_args_n op costs types_ f =
    let rec build_args i k =
      let cost = costs.(i) in
      function
      | [] -> k []
      | [ t ] -> search ~cost ~type_:t (fun v -> k [ v ])
      | t :: ts ->
          build_args (i + 1) (fun vs -> search ~cost ~type_:t (fun v -> k (v :: vs))) ts
    in
    build_args 0
      (fun args ->
        let state = Value.eval op @@ List.map ~f:to_value args in
        if not (Value.is_error state) then f (state, op, args))
      types_
  in
  let generate_args op costs f =
    match (costs, Op.args_type op) with
    | [| c |], [ t ] -> generate_args_1 op c t f
    | [| c1; c2 |], [ t1; t2 ] -> generate_args_2 op c1 t1 c2 t2 f
    | c, t -> generate_args_n op c t f
  in
  fun f ->
    let op_iter = Iter.of_list ops in
    if cost = 1 then
      op_iter
      |> Iter.filter (fun op -> Op.arity op = 0)
      |> Iter.iter (fun op -> f (Value.eval op [], op, []))
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
             costs (fun costs -> generate_args op costs f))

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

      let eval (op : Op.t) args =
        match (op, args) with
        | Plus, [ a; b ] -> a + b
        | Minus, [ a; b ] -> a - b
        | _ -> assert false

      let is_error _ = false
    end
  end in
  let open L in
  let search ~cost ~type_ =
    match (cost, (type_ : Type.t)) with
    | 1, Even -> Iter.singleton 0
    | 1, Odd -> Iter.singleton 1
    | _ -> assert false
  in
  let to_value = Fun.id in
  generate
    (module L : DSL
      with type Type.t = Type.t
       and type Value.t = Value.t
       and type Op.t = Op.t)
    search to_value [ L.Op.Plus; Minus ] 3
    (fun (value, op, args) ->
      print_s [%message (value : Value.t) (op : Op.t) (args : Value.t list)]);
  [%expect
    {|
    ((value 1) (op Plus) (args (0 1)))
    ((value 1) (op Minus) (args (1 0))) |}]
