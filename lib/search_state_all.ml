module type Lang_intf = sig
  module Type : sig
    type t [@@deriving compare, hash, sexp]
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
  module Value = struct
    include Lang.Value
    include Comparator.Make (Lang.Value)
  end

  module Attr = struct
    module T = struct
      type t = { cost : int; type_ : Lang.Type.t } [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)

    let create cost type_ = { cost; type_ }
  end

  module TValue = struct
    type t = { type_ : Lang.Type.t; value : Lang.Value.t } [@@deriving compare, hash, sexp]
  end

  type t = {
    values : Lang.Value.t Queue.t Hashtbl.M(Attr).t;
    paths : (int * Lang.Op.t * Lang.Value.t list) Queue.t Hashtbl.M(TValue).t;
  }
  [@@deriving sexp]

  let create () = { values = Hashtbl.create (module Attr); paths = Hashtbl.create (module TValue) }

  let search ctx ~cost ~type_ =
    assert (cost >= 0);
    match Hashtbl.find ctx.values @@ Attr.create cost type_ with Some q -> Queue.to_list q | None -> []

  let rec find_term ctx = function
    | Program.Apply (op, []) ->
        let all_values =
          Hashtbl.to_alist ctx.paths
          |> List.filter_map ~f:(fun (v, paths) ->
                 if Queue.exists ~f:(function _, op', [] -> [%compare.equal: Lang.Op.t] op op' | _ -> false) paths
                 then Some v.value
                 else None)
        in
        print_s [%message (op : Lang.Op.t) (List.length all_values : int)];

        Program.Apply ((op, all_values), [])
    | Apply (op, args) ->
        let args = List.map args ~f:(find_term ctx) in
        let arg_sets = List.map args ~f:(fun (Apply ((_, vs), _)) -> Set.of_list (module Value) vs) in
        let all_outputs =
          Hashtbl.to_alist ctx.paths
          |> List.filter_map ~f:(fun (v, paths) ->
                 if
                   Queue.exists
                     ~f:(function
                       | _, op', args ->
                           [%compare.equal: Lang.Op.t] op op' && List.for_all2_exn arg_sets args ~f:Set.mem)
                     paths
                 then Some v.value
                 else None)
        in
        print_s [%message (op : Lang.Op.t) (List.length all_outputs : int)];

        Apply ((op, all_outputs), args)

  let mem ctx = Hashtbl.mem ctx.paths

  let insert ctx cost state op inputs =
    let type_ = Lang.Op.ret_type op in
    let tvalue = TValue.{ type_; value = state } in
    (if not (mem ctx tvalue) then
     let q = Hashtbl.find_or_add ctx.values (Attr.create cost type_) ~default:Queue.create in
     Queue.enqueue q state);

    let paths = Hashtbl.find_or_add ctx.paths tvalue ~default:Queue.create in
    Queue.enqueue paths (cost, op, inputs)

  let insert_groups ctx cost groups =
    List.iter groups ~f:(fun states ->
        match List.permute states with
        | (value, op, args) :: states ->
            let type_ = Lang.Op.ret_type op in
            let tvalue = TValue.{ type_; value } in
            (if not (mem ctx tvalue) then
             let q = Hashtbl.find_or_add ctx.values { cost; type_ } ~default:Queue.create in
             Queue.enqueue q value);

            let paths = Hashtbl.find_or_add ctx.paths tvalue ~default:Queue.create in
            Queue.enqueue_all paths ((cost, op, args) :: List.map states ~f:(fun (_, op, args) -> (cost, op, args)))
        | [] -> ())

  let states ?cost ?type_ ctx =
    match (cost, type_) with
    | Some cost, Some type_ -> search ctx ~cost ~type_ |> Iter.of_list
    | Some cost, None ->
        fun f -> Hashtbl.iteri ctx.values ~f:(fun ~key ~data -> if key.cost = cost then Queue.iter data ~f)
    | None, Some type_ ->
        fun f ->
          Hashtbl.iteri ctx.values ~f:(fun ~key ~data ->
              if [%compare.equal: Lang.Type.t] key.type_ type_ then Queue.iter data ~f)
    | None, None -> Hashtbl.keys ctx.paths |> Iter.of_list |> Iter.map (fun v -> v.TValue.value)

  let length ctx = Hashtbl.length ctx.paths

  let print_stats ctx =
    Hashtbl.keys ctx.values
    |> List.map ~f:(fun a -> (a.cost, a.type_))
    |> List.sort ~compare:[%compare: int * Lang.Type.t]
    |> List.iter ~f:(fun (cost, type_) ->
           let n = Hashtbl.find_exn ctx.values { cost; type_ } |> Queue.length in
           eprint_s [%message (cost : int) (type_ : Lang.Type.t) (n : int)]);
    Fmt.epr "Total: %d\n%!" (length ctx)

  let print_contents ctx = print_s [%message (ctx.values : Lang.Value.t Queue.t Hashtbl.M(Attr).t)]

  let rec program_exn ctx type_ value =
    let _, op, args = Queue.peek_exn @@ Hashtbl.find_exn ctx.paths { type_; value } in
    program_of_op_args_exn ctx op args

  and program_of_op_args_exn ctx op args =
    Program.Apply (op, List.map2_exn (Lang.Op.args_type op) args ~f:(program_exn ctx))

  let rec random_program_exn ctx type_ value =
    let queue = Hashtbl.find_exn ctx.paths { type_; value } in
    let _, op, args = Queue.get queue (Random.int @@ Queue.length queue) in
    program_of_op_args_exn ctx op args

  and random_program_of_op_args_exn ctx op args =
    Program.Apply (op, List.map2_exn (Lang.Op.args_type op) args ~f:(random_program_exn ctx))

  let rec random_program_exn ctx max_cost type_ value =
    let q = Hashtbl.find_exn ctx.paths { type_; value } |> Queue.filter ~f:(fun (c, _, _) -> c <= max_cost) in
    let c, op, args = Queue.get q (Random.int @@ Queue.length q) in
    Program.Apply (op, List.map2_exn (Lang.Op.args_type op) args ~f:(random_program_exn ctx (c - 1)))

  let random_program_exn ?(max_cost = Int.max_value) ctx state = random_program_exn ctx max_cost state

  let clear { values; paths } =
    Hashtbl.clear values;
    Hashtbl.clear paths

  let cost_of ctx v =
    Hashtbl.to_alist ctx.values
    |> List.find_map ~f:(fun (k, vs) ->
           if Queue.mem vs v ~equal:[%compare.equal: Lang.Value.t] then Some k.cost else None)

  let n_states { values; _ } = Hashtbl.length values

  let n_transitions { paths; _ } = Hashtbl.fold ~init:0 ~f:(fun ~key:_ ~data sum -> sum + Queue.length data) paths
end
