open Std

module type Lang_intf = sig
  module Type : sig
    type t [@@deriving compare, hash, sexp]

    val default : t
    val output : t
  end

  module Op : sig
    type t [@@deriving compare, hash, sexp]

    val default : t
    val cost : t -> int
    val arity : t -> int
    val args_type : t -> Type.t list
    val ret_type : t -> Type.t
    val pp : t Fmt.t
  end

  module Value : sig
    type t [@@deriving compare, hash, sexp]

    val default : t
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

  module Class = struct
    module T = struct
      type t = { type_ : Type.t; value : Value.t } [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)

    let default = { type_ = Type.default; value = Value.default }
    let[@inline] create v t = { type_ = t; value = v }
    let[@inline] value c = c.value
    let[@inline] type_ c = c.type_
  end

  module Path0 = struct
    type t = {
      op : Op.t;
      args : Class.t list;
      value : Value.t;
      mutable cost : int;
      mutable height : int;
    }
    [@@deriving sexp]

    let default =
      { op = Op.default; args = []; value = Value.default; cost = -1; height = -1 }

    let[@inline] create value op args = { value; op; args; cost = -1; height = -1 }

    let pp =
      let open Fmt.Dump in
      record [ field "op" (fun x -> x.op) Op.pp ]
  end

  type 'a sek_e = 'a Sek.E.t

  let sek_e_of_sexp a_of_sexp s =
    let default, elems = [%of_sexp: a * a list] s in
    Sek.E.of_list default elems

  let sexp_of_sek_e sexp_of_a s = [%sexp_of: a * a list] Sek.E.(default s, to_list s)

  type paths = { paths : Path0.t sek_e; min_cost : int; min_height : int }
  [@@deriving sexp]

  type t = { classes : Class.t sek_e H.M(Attr).t; paths : paths H.M(Class).t }
  [@@deriving sexp]

  module Path : sig
    type ctx
    type t = Path0.t [@@deriving sexp]

    val create : Value.t -> Op.t -> Class.t list -> t
    val op : t -> Op.t
    val args : t -> Class.t list
    val value : t -> Value.t
    val cost : ctx -> t -> int
    val height : ctx -> t -> int
    val default : t
  end
  with type ctx := t = struct
    include Path0

    let op x = x.op
    let args x = x.args
    let value x = x.value

    let cost ctx p =
      (if p.cost < 0 then
       let args_cost =
         List.sum (module Int) p.args ~f:(fun c -> (H.find_exn ctx.paths c).min_cost)
       in
       p.cost <- Op.cost p.op + args_cost);
      p.cost

    let height ctx p =
      (if p.height < 0 then
       let args_height =
         List.map p.args ~f:(fun c -> (H.find_exn ctx.paths c).min_height)
         |> List.max_elt ~compare |> Option.value ~default:0
       in
       p.height <- 1 + args_height);
      p.height
  end

  let create () = { classes = H.create (module Attr); paths = H.create (module Class) }

  let to_channel ch { classes; paths } =
    let classes = H.to_alist classes and paths = H.to_alist paths in
    Marshal.to_channel ch (classes, paths) []

  let of_channel ch =
    let (classes, paths) =
      (Marshal.from_channel ch : (Attr.t * Class.t Sek.E.t) list * (Class.t * paths) list)
    in
    {
      classes = H.of_alist_exn (module Attr) classes;
      paths = H.of_alist_exn (module Class) paths;
    }

  let pp_dot fmt { classes; paths } =
    let node_ids =
      H.to_alist classes
      |> List.concat_map ~f:(fun (_, vs) -> Sek.E.to_list vs)
      |> List.mapi ~f:(fun i v -> (v, i))
      |> Hashtbl.of_alist_exn (module Class)
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
          Sek.E.fold_left
            (fun edges (path : Path.t) ->
              let args = Path.args path in
              let edge =
                (List.map args ~f:(H.find_exn node_ids), H.find_exn node_ids key)
              in
              match Map.add edges ~key:edge ~data:!id with
              | `Ok m ->
                  incr id;
                  m
              | _ -> edges)
            edges paths.paths)
        paths
    in
    Fmt.pf fmt "digraph {\n";
    Map.iteri hyper_edges ~f:(fun ~key:(args, out) ~data:id ->
        List.iter args ~f:(fun arg -> Fmt.pf fmt "%d -> %d;\n" arg id);
        Fmt.pf fmt "%d -> %d;\n" id out;
        Fmt.pf fmt "%d [shape=dot];\n" id);
    Fmt.pf fmt "}"

  let search_iter ctx ~cost ~type_ k =
    match H.find ctx.classes @@ Attr.create cost type_ with
    | Some q -> Sek.E.iter Sek.forward k q
    | None -> ()

  let search ctx ~cost ~type_ =
    search_iter ctx ~cost ~type_ |> Iter.map (fun c -> c.Class.value) |> Iter.to_list

  let rec find_term ctx = function
    | Apply (op, []) ->
        let pos_classes, neg_classes =
          H.to_alist ctx.paths
          |> List.partition_map ~f:(fun (v, (paths : paths)) ->
                 if
                   Sek.E.exists
                     (fun p ->
                       [%compare.equal: Op.t] op (Path.op p)
                       && List.is_empty (Path.args p))
                     paths.paths
                 then First v
                 else Second v)
        in
        Apply ((op, pos_classes, neg_classes), [])
    | Apply (op, args) ->
        let args = List.map args ~f:(find_term ctx) in
        let arg_sets =
          List.map args ~f:(fun (Apply ((_, vs, _), _)) -> Set.of_list (module Class) vs)
        in
        let pos_classes, neg_classes =
          H.to_alist ctx.paths
          |> List.partition_map ~f:(fun (v, (paths : paths)) ->
                 if
                   Sek.E.exists
                     (fun p ->
                       [%compare.equal: Op.t] op (Path.op p)
                       && List.for_all2_exn arg_sets (Path.args p) ~f:Set.mem)
                     paths.paths
                 then First v
                 else Second v)
        in
        Apply ((op, pos_classes, neg_classes), args)

  let mem_class ctx class_ = H.mem ctx.paths class_
  let classes ctx k = H.iter ctx.classes ~f:(Sek.E.iter Sek.forward k)

  let insert_class_members ctx class_ members =
    let new_paths =
      List.map ~f:(fun (value, op, args) -> Path.create value op args) members
      |> Sek.E.of_list Path.default
    in
    let old_paths = (H.find_exn ctx.paths class_).paths in
    Sek.E.append Sek.front old_paths @@ new_paths

  let insert_class ctx value op args =
    let args = List.map2_exn args (Op.args_type op) ~f:Class.create in
    let cost =
      let args_cost =
        List.sum (module Int) args ~f:(fun c -> (H.find_exn ctx.paths c).min_cost)
      in
      Op.cost op + args_cost
    in
    let height =
      let args_height =
        List.map args ~f:(fun c -> (H.find_exn ctx.paths c).min_height)
        |> List.max_elt ~compare |> Option.value ~default:0
      in
      1 + args_height
    in

    let class_ = Class.create value @@ Op.ret_type op in
    let classes =
      H.find_or_add ctx.classes (Attr.create cost class_.type_) ~default:(fun () ->
          Sek.E.create Class.default)
    in
    Sek.E.push Sek.front classes class_;

    let new_path = Path.create class_.value op args in
    let paths = Sek.E.of_list Path.default [ new_path ] in
    H.add_exn ctx.paths ~key:class_ ~data:{ min_cost = cost; min_height = height; paths }

  let length ctx = H.length ctx.paths

  let print_stats ctx =
    H.to_alist ctx.classes
    |> List.sort ~compare:(fun ((a : Attr.t), _) (a', _) ->
           [%compare: int * Type.t] (a.cost, a.type_) (a'.cost, a'.type_))
    |> List.iter ~f:(fun ((attr : Attr.t), classes) ->
           eprint_s
             [%message
               (attr.cost : int) (attr.type_ : Type.t) (Sek.E.length classes : int)]);
    Fmt.epr "Total: %d\n%!" (length ctx)

  let rec program_exn ctx max_height class_ =
    let p =
      Sek.E.find Sek.forward
        (fun p -> Path.height ctx p <= max_height)
        (H.find_exn ctx.paths class_).paths
    in
    program_of_op_args_exn ctx max_height p.op p.args

  and program_of_op_args_exn ctx max_height op args =
    Apply (op, List.map args ~f:(program_exn ctx (max_height - 1)))

  let rec random_program_exn ctx max_cost class_ =
    let low_cost_paths =
      (H.find_exn ctx.paths class_).paths
      |> Sek.E.filter (fun (p : Path.t) -> p.cost <= max_cost)
    in
    let p = Sek.E.get low_cost_paths (Random.int @@ Sek.E.length low_cost_paths) in
    Apply (p.op, List.map p.args ~f:(random_program_exn ctx (Path.cost ctx p - 1)))

  let random_program_exn ?(max_cost = Int.max_value) ctx state =
    random_program_exn ctx max_cost state

  let clear { classes; paths } =
    H.clear classes;
    H.clear paths

  let list_set l i x = List.mapi l ~f:(fun j y -> if i = j then x else y)

  let validate ss eval dist thresh =
    Hashtbl.iteri ss.paths ~f:(fun ~key ~data ->
        Sek.E.iter Sek.forward
          (fun (path : Path.t) ->
            let value = Path.value path in
            [%test_result: Value.t] ~message:"cached operator output" ~expect:value
              (eval path.op @@ List.map ~f:Class.value path.args);
            let d = dist key.value value in
            if d >. thresh then
              raise_s [%message "grouping not within threshold" (d : float)])
          data.paths)

  let rec local_greedy ss max_height eval dist (class_ : Class.t) =
    let open Option.Let_syntax in
    let all_paths = (H.find_exn ss.paths class_).paths in
    let eligible_paths =
      Sek.E.filter (fun (p : Path.t) -> Path.height ss p <= max_height) all_paths
    in
    let n_sample = max 1 (Sek.E.length eligible_paths / 2) in
    let%bind _, best_path =
      Iter.of_sek_e eligible_paths |> Iter.sample n_sample |> Iter.of_array
      |> Iter.map (fun (p : Path.t) -> (dist p.value, p))
      |> Iter.min ~lt:(fun (d, _) (d', _) -> Float.(d < d'))
    in
    let best_path_arg_values = List.map best_path.args ~f:(fun c -> c.value) in
    List.mapi best_path.args ~f:(fun i arg_class ->
        let dist' v = dist (eval best_path.op @@ list_set best_path_arg_values i v) in
        local_greedy ss (max_height - 1) eval dist' arg_class)
    |> Option.all
    |> Option.map ~f:(fun arg_progs -> Apply (best_path.op, arg_progs))

  let rec local_greedy_new ss max_height eval dist (class_ : Class.t) =
    let open Option.Let_syntax in
    let%bind _, best_path =
      let sampler = Sample.Incremental.weighted () in

      (H.find_exn ss.paths class_).paths
      |> Sek.E.iter Sek.forward (fun (p : Path.t) ->
             if Path.height ss p <= max_height then sampler.add p (1.0 -. dist p.value));
      sampler.get_sample ()
    in
    let best_path_arg_values = List.map best_path.args ~f:Class.value in
    let%bind args =
      List.mapi best_path.args ~f:(fun i arg_class ->
          let dist' v = dist (eval best_path.op @@ list_set best_path_arg_values i v) in
          local_greedy_new ss (max_height - 1) eval dist' arg_class)
      |> Option.all
    in
    return (Apply (best_path.op, args))

  let local_greedy =
    match Sys.getenv "METRIC_LOCAL_GREEDY" with
    | Some "new" -> local_greedy_new
    | _ -> local_greedy
end
