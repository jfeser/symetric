open Std
open Program.T

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

  let pp_dot fmt { values; paths } =
    let node_ids =
      H.to_alist values
      |> List.concat_map ~f:(fun (a, vs) ->
             Queue.to_list vs |> List.map ~f:(fun v -> TValue.{ value = v; type_ = a.type_ }))
      |> List.mapi ~f:(fun i v -> (v, i))
      |> Hashtbl.of_alist_exn (module TValue)
    in
    let module Edge = struct
      module T = struct
        type t = int list * int [@@deriving compare, sexp]
      end

      include T
      include Comparator.Make (T)
    end in
    let hyper_edges =
      let id = ref (Hashtbl.length node_ids) in
      H.fold
        ~init:(Map.empty (module Edge))
        ~f:(fun ~key ~data:paths edges ->
          Queue.fold ~init:edges
            ~f:(fun edges (path : Path.t) ->
              let edge =
                ( List.map2_exn (Op.args_type path.op) path.args ~f:(fun t v ->
                      H.find_exn node_ids TValue.{ type_ = t; value = v }),
                  H.find_exn node_ids key )
              in
              match Map.add edges ~key:edge ~data:!id with
              | `Ok m ->
                  incr id;
                  m
              | _ -> edges)
            paths)
        paths
    in
    Fmt.pf fmt "digraph {\n";
    Map.iteri hyper_edges ~f:(fun ~key:(args, out) ~data:id ->
        List.iter args ~f:(fun arg -> Fmt.pf fmt "%d -> %d;\n" arg id);
        Fmt.pf fmt "%d -> %d;\n" id out;
        Fmt.pf fmt "%d [shape=dot];\n" id);
    Fmt.pf fmt "}"

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

  and program_of_class_exn ctx (class_ : TValue.t) = program_exn ctx class_.type_ class_.value
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

  let local_search ?(max_tabu = 1000) ?(n_samples = 100) ss eval dist target size value op args =
    let eval = Program.eval (fun (op, _) args -> eval op args) in
    let module State = struct
      type t = (Op.t * Value.t) Program.t [@@deriving compare, hash, sexp]
    end in
    let open State in
    let rec random_program_exn ctx cost type_ value =
      let q = H.find_exn ctx.paths { type_; value } in
      let q = Queue.filter q ~f:(fun p -> p.cost <= cost) in
      Queue.to_list q |> List.permute
      |> List.find_map ~f:(fun p -> random_program_of_op_args ctx cost value p.op p.args)
    and random_program_of_op_args ctx cost value op args =
      Combinat.compositions ~n:(cost - 1) ~k:(List.length args)
      |> Iter.shuffle
      |> Iter.find (fun arg_costs ->
             List.map3_exn (Array.to_list arg_costs) (Op.args_type op) args ~f:(random_program_exn ctx)
             |> Option.all
             |> Option.map ~f:(fun args -> Apply ((op, value), args)))
    in
    let random_program_of_op_args_exn s c v o a = Option.value_exn (random_program_of_op_args s c v o a) in

    let neighbors (t : t) =
      let cmp = [%compare: float * _] in
      let size = Program.size t in
      Iter.forever (fun () ->
          try
            Some
              (replace (Random.int size)
                 (fun (Apply ((op, key), args) as p) ->
                   random_program_of_op_args_exn ss (Program.size p) key op
                     (List.map ~f:(fun (Apply ((_, key), _)) -> key) args))
                 t)
          with _ -> None)
      |> Iter.filter_map Fun.id |> Iter.take n_samples
      |> Iter.map (fun p -> (-.dist (eval p) target, p))
      |> Iter.top_k ~cmp (max_tabu + 1)
      |> Iter.map (fun (d, x) -> (-.d, x))
      |> Iter.sort ~cmp |> Iter.map Tuple.T2.get2
    in

    try
      Local_search.tabu ~max_tabu ~neighbors (module State) @@ random_program_of_op_args_exn ss size value op args
      |> Iter.map (Program.map ~f:(fun (op, _) -> op))
    with _ -> Iter.empty

  let local_search_tv ?(max_tabu = 1000) ?(n_samples = 10) ss eval dist target size type_ value =
    let eval = Program.eval (fun (op, _) args -> eval op args) in
    let module State = struct
      type t = (Op.t * Value.t) Program.t [@@deriving compare, hash, sexp]
    end in
    let open State in
    let rec random_program_exn ctx cost type_ value =
      let q = H.find_exn ctx.paths { type_; value } in
      let q = Queue.filter q ~f:(fun p -> p.cost <= cost) in
      Queue.to_list q |> List.permute
      |> List.find_map ~f:(fun p -> random_program_of_op_args ctx cost value p.op p.args)
    and random_program_of_op_args ctx cost value op args =
      if List.is_empty args then Some (Apply ((op, value), []))
      else
        Combinat.compositions ~n:(cost - 1) ~k:(List.length args)
        |> Iter.map Array.to_list |> Iter.shuffle
        |> Iter.find (fun arg_costs ->
               List.map3_exn arg_costs (Op.args_type op) args ~f:(random_program_exn ctx)
               |> Option.all
               |> Option.map ~f:(fun args -> Apply ((op, value), args)))
    in

    let random_program_of_op_args_exn s c v o a = Option.value_exn (random_program_of_op_args s c v o a) in

    let neighbors (t : t) =
      let cmp = [%compare: float * _] in
      let size = Program.size t in
      let ns =
        Iter.forever (fun () ->
            replace (Random.int size)
              (fun (Apply ((op, key), args) as p) ->
                random_program_of_op_args_exn ss (Program.size p) key op
                  (List.map ~f:(fun (Apply ((_, key), _)) -> key) args))
              t)
        |> Iter.filter (fun p -> not ([%compare.equal: (Op.t * Value.t) Program.t] t p))
        |> Iter.take n_samples
        |> Iter.map (fun p -> (-.dist (eval p) target, p))
        |> Iter.top_k ~cmp (max_tabu + 1)
        |> Iter.map (fun (d, x) -> (-.d, x))
        |> Iter.to_list
      in
      (* print_s [%message (ns : (float * (Op.t * _) Program.t) list)]; *)
      Iter.of_list ns |> Iter.sort ~cmp |> Iter.map (fun (d, x) -> (d, x)) |> Iter.map Tuple.T2.get2
    in

    let start_prog = Option.value_exn (random_program_exn ss size type_ value) in
    print_s [%message (start_prog : (Op.t * _) Program.t)];
    Local_search.tabu ~max_tabu ~neighbors (module State) start_prog |> Iter.map (Program.map ~f:(fun (op, _) -> op))

  let rec all_map ~f =
    let open Option.Let_syntax in
    function
    | [] -> return []
    | x :: xs ->
        let%bind x' = f x in
        let%bind xs' = all_map ~f xs in
        return (x' :: xs')

  let rec local_search_direct_ ss eval dist target (class_ : TValue.t) =
    if [%compare.equal: Value.t] target class_.value then Some (program_of_class_exn ss class_)
    else
      let paths = H.find_exn ss.paths class_ in
      let solution_m =
        Queue.find_map paths ~f:(fun (p : Path.t) ->
            if [%compare.equal: Value.t] target p.value then
              let arg_progs =
                List.map2_exn p.args (Op.args_type p.op) ~f:(fun v t ->
                    program_of_class_exn ss { value = v; type_ = t })
              in
              Some (Apply (p.op, arg_progs))
            else None)
      in
      match solution_m with
      | Some s -> Some s
      | None ->
          print_s [%message (Queue.length paths : int)];
          Iter.of_queue paths
          |> Iter.map (fun (p : Path.t) -> (dist target p.value, p))
          |> Iter.sort ~cmp:(fun (d, _) (d', _) -> [%compare: float] d d')
          |> Iter.find_map (fun (_, (p : Path.t)) ->
                 List.map2_exn p.args (Op.args_type p.op) ~f:(fun v t ->
                     let class_ : TValue.t = { value = v; type_ = t } in
                     H.find_exn ss.paths class_ |> Iter.of_queue |> Iter.map (fun (p : Path.t) -> (p.value, class_)))
                 |> Iter.list_product
                 |> Iter.find_map (fun args' ->
                        let args, classes = List.unzip args' in
                        if [%compare.equal: Value.t] target (eval p.op args) then
                          all_map args' ~f:(fun (v, c) -> local_search_direct ss eval dist v c)
                        else None)
                 |> Option.map ~f:(fun arg_progs -> Apply (p.op, arg_progs)))

  and local_search_direct ss eval dist target class_ =
    let ret = local_search_direct_ ss eval dist target class_ in
    print_s [%message (class_ : TValue.t) (ret : Op.t Program.t option)];
    ret

  let list_set l i x = List.mapi l ~f:(fun j y -> if i = j then x else y)

  (* let rec local_search2 ss cost eval dist (class_ : TValue.t) : _ Gen.t = *)
  (*   let mk_heap () = Pairing_heap.create ~cmp:(fun (d, _) (d', _) -> [%compare: float] d d') () in *)
  (*   let path_heap = mk_heap () in *)
  (*   H.find_exn ss.paths class_ *)
  (*   |> Queue.iter ~f:(fun p -> if p.cost <= cost then Pairing_heap.add path_heap (dist p.value, (p, None))); *)
  (*   let rec path_candidates () = *)
  (*     let path = Pairing_heap.pop path_heap in *)
  (*     match path with *)
  (*     | None -> None *)
  (*     | Some (d, (({ args = [] } as path), None)) -> Some (d, Apply (path.op, [])) *)
  (*     | Some (d, (({ args = [ args_value ] } as path), None)) -> *)
  (*         let args_type = match Op.args_type path.op with [ t ] -> t | _ -> assert false in *)
  (*         let dist' v = dist (eval path.op [ v ]) in *)
  (*         let alts = *)
  (*           local_search2 ss (cost - 1) eval dist' { value = args_value; type_ = args_type } *)
  (*           |> Gen.map (fun (d', p) -> (d', Apply (path.op, [ p ]))) *)
  (*         in *)
  (*         Pairing_heap.add path_heap (d, (path, Some alts)); *)
  (*         path_candidates () *)
  (*     | Some (d, (path, None)) -> *)
  (*         let arg_alts = *)
  (*           let i = ref 0 in *)
  (*           List.map2_exn (Op.args_type path.op) path.args ~f:(fun arg_type arg_value -> *)
  (*               let arg_class = TValue.{ value = arg_value; type_ = arg_type } in *)
  (*               let idx = !i in *)
  (*               let dist' v = dist (eval path.op @@ list_set path.args idx v) in *)
  (*               incr i; *)
  (*               Gen.peek @@ local_search2 ss (cost - 1) eval dist' arg_class) *)
  (*         in *)
  (*         let alts = *)
  (*           List.map arg_alts ~f:(fun alts -> *)
  (*               let current = Option.value_exn (Gen.next alts) in *)
  (*               let next = Gen.next alts in *)
  (*               (current, next, alts)) *)
  (*             |>  *)
  (*           Gen.unfold (fun arg_alts -> *)
  (*               let idx = List.mapi arg_alts ~f:(function *)
  (*                       | ((d, _), Some (d', _), _) -> d)) *)

  (*             |> Gen.list_product *)
  (*           |> Gen.map (fun args -> *)
  (*                  let _, args = List.unzip args in *)
  (*                  let p = Apply (path.op, args) in *)
  (*                  (dist (Program.eval eval p), p)) *)
  (*         in *)
  (*         Pairing_heap.add path_heap (d, (path, Some alts)); *)
  (*         path_candidates () *)
  (*     | Some (_, (path, Some alts)) -> ( *)
  (*         match alts () with *)
  (*         | None -> path_candidates () *)
  (*         | Some (d, x) -> *)
  (*             Pairing_heap.add path_heap (d, (path, Some alts)); *)
  (*             Some (d, x)) *)
  (*   in *)
  (*   path_candidates *)

  let to_grammar ss op_pp (class_ : TValue.t) =
    let syms = Hashtbl.create (module TValue) in
    let id = ref 0 in
    let sym_of class_ =
      match Hashtbl.find syms class_ with
      | Some s -> s
      | None ->
          let sym = sprintf "x%d" !id in
          incr id;
          Hashtbl.set syms ~key:class_ ~data:sym;
          sym
    in
    let printed = Hash_set.create (module TValue) in
    let to_print = Hash_set.create (module TValue) in

    let rec to_grammar () =
      match Hash_set.find to_print ~f:(Fun.const true) with
      | Some class_ when not (Hash_set.mem printed class_) ->
          Hash_set.add printed class_;
          Hash_set.remove to_print class_;

          Fmt.pr "@[<hov 4>%s :=@ " @@ sym_of class_;
          Hashtbl.find_exn ss.paths class_
          |> Queue.iter ~f:(fun p ->
                 Fmt.pr "@[<hv>%a(" op_pp p.op;
                 Fmt.pr "%a)@]@ | " (Fmt.list ~sep:Fmt.comma Fmt.string)
                 @@ List.map2_exn (Op.args_type p.op) p.args ~f:(fun arg_type arg_value ->
                        let arg_class = TValue.{ value = arg_value; type_ = arg_type } in
                        if not (Hash_set.mem printed arg_class) then Hash_set.add to_print arg_class;
                        sym_of arg_class));
          Fmt.pr "@]@,";
          to_grammar ()
      | Some _ -> to_grammar ()
      | None -> ()
    in

    Hash_set.add to_print class_;
    Fmt.pr "@[<v>";
    to_grammar ();
    Fmt.pr "@]"

  let rec local_greedy ss cost eval dist (class_ : TValue.t) =
    let open Option.Let_syntax in
    let%bind _, best_path =
      let all_paths = H.find_exn ss.paths class_ in
      let eligible_paths =
        Iter.of_queue all_paths |> Iter.filter (fun (p : Path.t) -> cost > 1 || List.is_empty p.args) |> Iter.to_list
      in
      let n_sample = max 1 (List.length eligible_paths / 2) in
      Iter.of_list eligible_paths |> Iter.sample n_sample |> Iter.of_array
      |> Iter.map (fun (p : Path.t) -> (dist p.value, p))
      |> Iter.min ~lt:(fun (d, _) (d', _) -> Float.(d < d'))
    in
    List.zip_exn (Op.args_type best_path.op) best_path.args
    |> List.mapi ~f:(fun i (arg_type, arg_value) ->
           let arg_class = TValue.{ value = arg_value; type_ = arg_type } in
           let dist' v = dist (eval best_path.op @@ list_set best_path.args i v) in
           local_greedy ss (cost - 1) eval dist' arg_class)
    |> Option.all
    |> Option.map ~f:(fun arg_progs -> Apply (best_path.op, arg_progs))
end
