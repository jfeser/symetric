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
    val pp : t Fmt.t
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
    type t = { cost : int; height : int; op : Op.t; args : Value.t list; value : Value.t } [@@deriving sexp]

    let pp =
      let open Fmt in
      let open Fmt.Dump in
      record [ field "cost" (fun x -> x.cost) int; field "op" (fun x -> x.op) Op.pp ]
  end

  type paths = { paths : Path.t list; min_cost : int; min_height : int } [@@deriving sexp]
  type t = { values : Value.t Queue.t H.M(Attr).t; paths : paths H.M(TValue).t } [@@deriving sexp]

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
          List.fold ~init:edges
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
            paths.paths)
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
                   List.exists
                     ~f:(function { op = op'; args = []; _ } -> [%compare.equal: Op.t] op op' | _ -> false)
                     paths.paths
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
                   List.exists
                     ~f:(function
                       | { op = op'; args; _ } ->
                           [%compare.equal: Op.t] op op' && List.for_all2_exn arg_sets args ~f:Set.mem)
                     paths.paths
                 then Some v.value
                 else None)
        in
        print_s [%message (op : Op.t) (List.length all_outputs : int)];

        Apply ((op, all_outputs), args)

  let mem ctx = H.mem ctx.paths

  let insert ?cost ?height ?key ctx state op inputs =
    let key = Option.value key ~default:state in
    let type_ = Op.ret_type op in
    let tvalue = TValue.{ type_; value = key } in
    let cost =
      match cost with
      | Some c -> c
      | None ->
          let args_cost =
            List.zip_exn (Op.args_type op) inputs
            |> List.sum (module Int) ~f:(fun (t, v) -> (H.find_exn ctx.paths TValue.{ type_ = t; value = v }).min_cost)
          in
          Op.cost op + args_cost
    in
    let height =
      match height with
      | Some h -> h
      | None ->
          let args_height =
            List.zip_exn (Op.args_type op) inputs
            |> Iter.of_list
            |> Iter.map (fun (t, v) -> (H.find_exn ctx.paths TValue.{ type_ = t; value = v }).min_height)
            |> Iter.max ~lt:( < ) |> Option.value ~default:0
          in
          1 + args_height
    in

    (if not (mem ctx tvalue) then
     let q = H.find_or_add ctx.values (Attr.create cost type_) ~default:Queue.create in
     Queue.enqueue q state);
    let new_path = Path.{ cost; height; op; args = inputs; value = state } in
    H.update ctx.paths tvalue ~f:(function
      | Some ps -> { ps with paths = new_path :: ps.paths }
      | None -> { min_cost = cost; min_height = height; paths = [ new_path ] })

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

  let rec program_exn ctx max_height type_ value =
    let p = List.find_exn ~f:(fun p -> p.height <= max_height) (H.find_exn ctx.paths { type_; value }).paths in
    program_of_op_args_exn ctx max_height p.op p.args

  and program_of_class_exn ctx max_height (class_ : TValue.t) = program_exn ctx max_height class_.type_ class_.value

  and program_of_op_args_exn ctx max_height op args =
    Apply (op, List.map2_exn (Op.args_type op) args ~f:(program_exn ctx (max_height - 1)))

  let rec random_program_exn ctx max_height type_ value =
    let p =
      List.random_element_exn
      @@ List.filter ~f:(fun p -> p.height <= max_height) (H.find_exn ctx.paths { type_; value }).paths
    in
    random_program_of_op_args_exn ctx max_height p.op p.args

  and random_program_of_op_args_exn ctx max_height op args =
    Apply (op, List.map2_exn (Op.args_type op) args ~f:(random_program_exn ctx (max_height - 1)))

  let rec random_program_exn ctx max_cost type_ value =
    let p =
      (H.find_exn ctx.paths { type_; value }).paths
      |> List.filter ~f:(fun p -> p.cost <= max_cost)
      |> List.random_element_exn
    in
    Apply (p.op, List.map2_exn (Op.args_type p.op) p.args ~f:(random_program_exn ctx (p.cost - 1)))

  let random_program_exn ?(max_cost = Int.max_value) ctx state = random_program_exn ctx max_cost state

  let clear { values; paths } =
    H.clear values;
    H.clear paths

  let cost_of ctx v =
    H.to_alist ctx.values
    |> List.find_map ~f:(fun (k, vs) -> if Queue.mem vs v ~equal:[%compare.equal: Value.t] then Some k.cost else None)

  let n_states { values; _ } = H.length values
  let n_transitions { paths; _ } = H.fold ~init:0 ~f:(fun ~key:_ ~data sum -> sum + List.length data.paths) paths

  let replace i f prog =
    let rec replace j p =
      if i = j then f p
      else
        let (Apply (op, args)) = p in
        Apply (op, List.mapi args ~f:(fun k p' -> replace (j + k) p'))
    in
    replace 0 prog

  let list_set l i x = List.mapi l ~f:(fun j y -> if i = j then x else y)

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
          (Hashtbl.find_exn ss.paths class_).paths
          |> List.iter ~f:(fun p ->
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

  let rec local_greedy value_pp ss max_height eval dist (class_ : TValue.t) =
    let open Option.Let_syntax in
    let all_paths = (H.find_exn ss.paths class_).paths in
    assert (List.exists all_paths ~f:(fun p -> [%compare.equal: Value.t] p.value class_.value));
    let eligible_paths =
      Iter.of_list all_paths |> Iter.filter (fun (p : Path.t) -> p.height <= max_height) |> Iter.to_list
    in
    let n_sample = max 1 (List.length eligible_paths / 2) in
    let%bind _, best_path =
      Iter.of_list eligible_paths |> Iter.sample n_sample |> Iter.of_array
      |> Iter.map (fun (p : Path.t) -> (dist p.value, p))
      |> Iter.min ~lt:(fun (d, _) (d', _) -> Float.(d < d'))
    in
    List.zip_exn (Op.args_type best_path.op) best_path.args
    |> List.mapi ~f:(fun i (arg_type, arg_value) ->
           let arg_class = TValue.{ value = arg_value; type_ = arg_type } in
           let dist' v = dist (eval best_path.op @@ list_set best_path.args i v) in
           local_greedy value_pp ss (max_height - 1) eval dist' arg_class)
    |> Option.all
    |> Option.map ~f:(fun arg_progs -> Apply (best_path.op, arg_progs))
end
