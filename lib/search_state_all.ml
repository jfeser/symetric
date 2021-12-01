open Std

module type Lang_intf = sig
  module Type : sig
    type t [@@deriving compare, hash, sexp]

    val output : t
  end

  module Op : sig
    type t [@@deriving compare, hash, sexp]

    val cost : t -> int

    val arity : t -> int

    val args_type : t -> Type.t list

    val ret_type : t -> Type.t
  end

  module Value : sig
    type t [@@deriving compare, hash, sexp]
  end
end

module Make (Lang : Lang_intf) = struct
  open Lang
  open Program.T
  module H = Hashtbl

  module Value = struct
    include Lang.Value
    include Comparator.Make (Lang.Value)
  end

  module Attr = struct
    module T = struct
      type t = { cost : int; type_ : Type.t } [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)

    let create cost type_ = { cost; type_ }
  end

  module TValue = struct
    type t = { type_ : Type.t; value : Value.t } [@@deriving compare, hash, sexp]
  end

  module Path = struct
    type t = { cost : int; op : Op.t; args : Value.t list; value : Value.t } [@@deriving sexp]
  end

  type t = { values : Value.t Queue.t H.M(Attr).t; paths : Path.t Queue.t H.M(TValue).t } [@@deriving sexp]

  let create () = { values = H.create (module Attr); paths = H.create (module TValue) }

  let search ctx ~cost ~type_ =
    assert (cost >= 0);
    match H.find ctx.values @@ Attr.create cost type_ with Some q -> Queue.to_list q | None -> []

  let rec find_term ctx = function
    | Apply (op, []) ->
        let all_values =
          H.to_alist ctx.paths
          |> List.filter_map ~f:(fun (v, paths) ->
                 if
                   Queue.exists
                     ~f:(function { op = op'; args = []; _ } -> [%compare.equal: Op.t] op op' | _ -> false)
                     paths
                 then Some v.value
                 else None)
        in
        print_s [%message (op : Op.t) (List.length all_values : int)];

        Apply ((op, all_values), [])
    | Apply (op, args) ->
        let args = List.map args ~f:(find_term ctx) in
        let arg_sets = List.map args ~f:(fun (Apply ((_, vs), _)) -> Set.of_list (module Value) vs) in
        let all_outputs =
          H.to_alist ctx.paths
          |> List.filter_map ~f:(fun (v, paths) ->
                 if
                   Queue.exists
                     ~f:(function
                       | { op = op'; args; _ } ->
                           [%compare.equal: Op.t] op op' && List.for_all2_exn arg_sets args ~f:Set.mem)
                     paths
                 then Some v.value
                 else None)
        in
        print_s [%message (op : Op.t) (List.length all_outputs : int)];

        Apply ((op, all_outputs), args)

  let mem ctx = H.mem ctx.paths

  let insert ?key ctx cost state op inputs =
    let type_ = Op.ret_type op in
    let key = Option.value key ~default:state in
    let tvalue = TValue.{ type_; value = key } in
    (if not (mem ctx tvalue) then
     let q = H.find_or_add ctx.values (Attr.create cost type_) ~default:Queue.create in
     Queue.enqueue q state);
    let paths = H.find_or_add ctx.paths tvalue ~default:Queue.create in
    Queue.enqueue paths { cost; op; args = inputs; value = state }

  let states ?cost ?type_ ctx =
    match (cost, type_) with
    | Some cost, Some type_ -> search ctx ~cost ~type_ |> Iter.of_list
    | Some cost, None -> fun f -> H.iteri ctx.values ~f:(fun ~key ~data -> if key.cost = cost then Queue.iter data ~f)
    | None, Some type_ ->
        fun f ->
          H.iteri ctx.values ~f:(fun ~key ~data -> if [%compare.equal: Type.t] key.type_ type_ then Queue.iter data ~f)
    | None, None -> H.keys ctx.paths |> Iter.of_list |> Iter.map (fun v -> v.TValue.value)

  let length ctx = H.length ctx.paths

  let print_stats ctx =
    H.keys ctx.values
    |> List.map ~f:(fun a -> (a.cost, a.type_))
    |> List.sort ~compare:[%compare: int * Type.t]
    |> List.iter ~f:(fun (cost, type_) ->
           let n = H.find_exn ctx.values { cost; type_ } |> Queue.length in
           eprint_s [%message (cost : int) (type_ : Type.t) (n : int)]);
    Fmt.epr "Total: %d\n%!" (length ctx)

  let print_contents ctx = print_s [%message (ctx.values : Value.t Queue.t H.M(Attr).t)]

  let rec program_exn ctx type_ value =
    let p = Queue.peek_exn @@ H.find_exn ctx.paths { type_; value } in
    program_of_op_args_exn ctx p.op p.args

  and program_of_op_args_exn ctx op args = Apply (op, List.map2_exn (Op.args_type op) args ~f:(program_exn ctx))

  let rec random_program_exn ctx type_ value =
    let queue = H.find_exn ctx.paths { type_; value } in
    let p = Queue.get queue (Random.int @@ Queue.length queue) in
    program_of_op_args_exn ctx p.op p.args

  and random_program_of_op_args_exn ctx op args =
    Apply (op, List.map2_exn (Op.args_type op) args ~f:(random_program_exn ctx))

  let rec random_program_exn ctx max_cost type_ value =
    let q = H.find_exn ctx.paths { type_; value } |> Queue.filter ~f:(fun p -> p.cost <= max_cost) in
    let p = Queue.get q (Random.int @@ Queue.length q) in
    Apply (p.op, List.map2_exn (Op.args_type p.op) p.args ~f:(random_program_exn ctx (p.cost - 1)))

  let random_program_exn ?(max_cost = Int.max_value) ctx state = random_program_exn ctx max_cost state

  let clear { values; paths } =
    H.clear values;
    H.clear paths

  let cost_of ctx v =
    H.to_alist ctx.values
    |> List.find_map ~f:(fun (k, vs) -> if Queue.mem vs v ~equal:[%compare.equal: Value.t] then Some k.cost else None)

  let n_states { values; _ } = H.length values

  let n_transitions { paths; _ } = H.fold ~init:0 ~f:(fun ~key:_ ~data sum -> sum + Queue.length data) paths

  let replace i f prog =
    let rec replace j p =
      if i = j then f p
      else
        let (Apply (op, args)) = p in
        Apply (op, List.mapi args ~f:(fun k p' -> replace (j + k) p'))
    in
    replace 0 prog

  let local_search ?(max_tabu = 1000) ?(n_samples = 10) ss eval dist target size value op args =
    let eval = Program.eval (fun (op, _) args -> eval op args) in
    let module State = struct
      type t = (Op.t * Value.t) Program.t [@@deriving compare, hash, sexp]
    end in
    let open State in
    let rec random_program_exn ctx max_depth type_ value =
      let q = H.find_exn ctx.paths { type_; value } in
      let q = if max_depth = 1 then Queue.filter q ~f:(fun p -> List.is_empty p.args) else q in
      let p = Queue.get q (Random.int @@ Queue.length q) in
      random_program_of_op_args_exn ctx max_depth value p.op p.args
    and random_program_of_op_args_exn ctx max_depth value op args =
      Apply ((op, value), List.map2_exn (Op.args_type op) args ~f:(random_program_exn ctx (max_depth - 1)))
    in

    let neighbors (t : t) =
      let cmp = [%compare: float * _] in
      let size = Program.size t in
      Iter.forever (fun () ->
          replace (Random.int size)
            (fun (Apply ((op, key), args) as p) ->
              random_program_of_op_args_exn ss (Program.size p) key op
                (List.map ~f:(fun (Apply ((_, key), _)) -> key) args))
            t)
      |> Iter.take n_samples
      |> Iter.map (fun p -> (-.dist (eval p) target, p))
      |> Iter.top_k ~cmp (max_tabu + 1)
      |> Iter.map (fun (d, x) -> (-.d, x))
      |> Iter.sort ~cmp |> Iter.map Tuple.T2.get2
    in

    Local_search.tabu ~max_tabu ~neighbors (module State) @@ random_program_of_op_args_exn ss size value op args
    |> Iter.map (Program.map ~f:(fun (op, _) -> op))
end
