open Std

module type DSL = sig
  module Type : sig
    type t [@@deriving compare, equal, hash, sexp]
  end

  module Op : sig
    type t [@@deriving compare, equal, hash, sexp]

    val pp : t Fmt.t
  end

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp]
  end
end

module Make (Lang : DSL) = struct
  open Lang
  open Program.T
  module H = Hashtbl

  module Value = struct
    module T = struct
      type t = Ancient of Lang.Value.t Ancient.ancient | Standard of Lang.Value.t

      let to_lang_value = function Ancient v -> Ancient.follow v | Standard v -> v
      let compare x y = Lang.Value.compare (to_lang_value x) (to_lang_value y)
      let sexp_of_t x = Lang.Value.sexp_of_t (to_lang_value x)
    end

    include T
    include Comparator.Make (T)

    let equal x y = compare x y = 0
    let hash x = Lang.Value.hash (to_lang_value x)
    let hash_fold_t s x = Lang.Value.hash_fold_t s (to_lang_value x)

    let of_lang_value v =
      if Ancient.is_ancient v then Standard v else Ancient (Ancient.mark v)

    let t_of_sexp x = of_lang_value (Lang.Value.t_of_sexp x)
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
      type t = { attr : Attr.t; value : Value.t } [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)

    let[@inline] create t c v =
      { attr = { type_ = t; cost = c }; value = Value.of_lang_value v }

    let[@inline] create' attr v = { attr; value = Value.of_lang_value v }
    let[@inline] value c = Value.to_lang_value c.value
    let[@inline] cost c = c.attr.cost
    let[@inline] type_ c = c.attr.type_
  end

  type path = { op : Op.t; args : Class.t list; ret : Class.t; height : int Lazy.t }
  [@@deriving sexp]

  type t = { classes : Class.t list H.M(Attr).t; paths : path list H.M(Class).t }
  [@@deriving sexp]

  module Path = struct
    type t = path [@@deriving sexp]

    let[@inline] create ret op args =
      { ret; op; args; height = lazy (failwith "undefined") }

    let pp =
      let open Fmt.Dump in
      record [ field "op" (fun x -> x.op) Op.pp ]

    let op x = x.op
    let args x = x.args
    let value x = Class.value x.ret
    let cost p = Class.cost p.ret
    let height p = Lazy.force p.height
  end

  let create () = { classes = H.create (module Attr); paths = H.create (module Class) }

  let to_channel ch { classes; paths } =
    let classes = H.to_alist classes and paths = H.to_alist paths in
    Marshal.to_channel ch (classes, paths) []

  let of_channel ch =
    let (classes, paths) =
      (Marshal.from_channel ch
        : (Attr.t * Class.t list) list * (Class.t * Path.t list) list)
    in
    {
      classes = H.of_alist_exn (module Attr) classes;
      paths = H.of_alist_exn (module Class) paths;
    }

  let pp_dot fmt { classes; paths } =
    let node_ids =
      H.to_alist classes
      |> List.concat_map ~f:(fun (_, vs) -> vs)
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
          List.fold_left
            ~f:(fun edges (path : Path.t) ->
              let args = Path.args path in
              let edge =
                (List.map args ~f:(H.find_exn node_ids), H.find_exn node_ids key)
              in
              match Map.add edges ~key:edge ~data:!id with
              | `Ok m ->
                  incr id;
                  m
              | _ -> edges)
            ~init:edges paths)
        paths
    in
    Fmt.pf fmt "digraph {\n";
    Map.iteri hyper_edges ~f:(fun ~key:(args, out) ~data:id ->
        List.iter args ~f:(fun arg -> Fmt.pf fmt "%d -> %d;\n" arg id);
        Fmt.pf fmt "%d -> %d;\n" id out;
        Fmt.pf fmt "%d [shape=dot];\n" id);
    Fmt.pf fmt "}"

  let search_iter ?type_ ~cost ctx k =
    match type_ with
    | Some type_ -> (
        match H.find ctx.classes @@ Attr.create cost type_ with
        | Some q -> List.iter ~f:k q
        | None -> ())
    | None ->
        H.iteri ctx.classes ~f:(fun ~key ~data ->
            if key.cost = cost then List.iter ~f:k data)

  let search ctx ~cost ~type_ =
    search_iter ctx ~cost ~type_ |> Iter.map Class.value |> Iter.to_list

  let find_leaf_term ctx op =
    let classes =
      H.to_alist ctx.paths
      |> List.filter_map ~f:(fun (v, paths) ->
             Option.some_if
               (List.exists
                  ~f:(fun p ->
                    [%compare.equal: Op.t] op (Path.op p) && List.is_empty (Path.args p))
                  paths)
               v)
    in
    Apply ((op, classes), [])

  let find_nonleaf_term find_term ctx op args =
    let args = List.map args ~f:(find_term ctx) in
    let arg_sets =
      List.map args ~f:(fun (Apply ((_, vs), _)) -> Set.of_list (module Class) vs)
    in
    let pos_classes =
      H.to_alist ctx.paths
      |> List.filter_map ~f:(fun (v, paths) ->
             Option.some_if
               (List.exists
                  ~f:(fun p ->
                    [%compare.equal: Op.t] op (Path.op p)
                    && List.for_all2_exn arg_sets (Path.args p) ~f:Set.mem)
                  paths)
               v)
    in
    Apply ((op, pos_classes), args)

  let rec find_term ctx = function
    | Apply (op, []) -> find_leaf_term ctx op
    | Apply (op, args) -> find_nonleaf_term find_term ctx op args

  let mem_class ctx class_ = H.mem ctx.paths class_

  let mem ctx type_ cost value =
    H.mem ctx.paths { attr = { type_; cost }; value = Standard value }

  let classes ctx k = H.iter ctx.classes ~f:(List.iter ~f:k)

  let insert_class_members ctx class_ members =
    H.update ctx.paths class_ ~f:(function
      | Some old_paths ->
          List.fold_left members ~init:old_paths ~f:(fun paths (value, op, args) ->
              Path.create
                { attr = class_.attr; value = Value.of_lang_value value }
                op args
              :: paths)
      | None -> raise_s [%message "no such class" (class_ : Class.t)])

  let insert_class ctx type_ cost value op args =
    let height =
      lazy
        (let args_height =
           List.map args ~f:(fun c ->
               H.find_exn ctx.paths c |> Iter.of_list |> Iter.map Path.height |> Iter.max
               |> Option.value ~default:0)
           |> List.max_elt ~compare |> Option.value ~default:0
         in
         1 + args_height)
    in

    let insert_path (class_ : Class.t) =
      let new_path = { op; args; ret = class_; height } in
      H.update ctx.paths class_ ~f:(function
        | Some paths -> new_path :: paths
        | None -> [ new_path ])
    in
    H.findi_and_call ctx.classes { Attr.cost; type_ }
      ~if_found:(fun ~key:attr ~data:classes ->
        let class_ = Class.create' attr value in
        H.set ctx.classes ~key:attr ~data:(class_ :: classes);
        insert_path class_)
      ~if_not_found:(fun attr ->
        let class_ = Class.create' attr value in
        H.set ctx.classes ~key:attr ~data:[ class_ ];
        insert_path class_)

  let insert_class ctx type_ cost value op args =
    let height =
      lazy
        (let args_height =
           List.map args ~f:(fun c ->
               H.find_exn ctx.paths c |> Iter.of_list |> Iter.map Path.height |> Iter.max
               |> Option.value ~default:0)
           |> List.max_elt ~compare |> Option.value ~default:0
         in
         1 + args_height)
    in

    let insert_path (class_ : Class.t) =
      let new_path = { op; args; ret = class_; height } in
      H.update ctx.paths class_ ~f:(function
        | Some paths -> new_path :: paths
        | None -> [ new_path ])
    in
    H.findi_and_call ctx.classes { Attr.cost; type_ }
      ~if_found:(fun ~key:attr ~data:classes ->
        let class_ = Class.create' attr value in
        H.set ctx.classes ~key:attr ~data:(class_ :: classes);
        insert_path class_)
      ~if_not_found:(fun attr ->
        let class_ = Class.create' attr value in
        H.set ctx.classes ~key:attr ~data:[ class_ ];
        insert_path class_)

  let length ctx = H.length ctx.paths

  let print_stats ctx =
    H.to_alist ctx.classes
    |> List.sort ~compare:(fun ((a : Attr.t), _) (a', _) ->
           [%compare: int * Type.t] (a.cost, a.type_) (a'.cost, a'.type_))
    |> List.iter ~f:(fun ((attr : Attr.t), classes) ->
           eprint_s
             [%message
               (attr.cost : int) (attr.type_ : Type.t) (List.length classes : int)]);
    Fmt.epr "Total: %d\n%!" (length ctx)

  let rec program_exn ctx max_height class_ =
    let p =
      List.find_exn
        ~f:(fun p -> Path.height p <= max_height)
        (H.find_exn ctx.paths class_)
    in
    program_of_op_args_exn ctx max_height p.op p.args

  and program_of_op_args_exn ctx max_height op args =
    Apply (op, List.map args ~f:(program_exn ctx (max_height - 1)))

  let rec random_program_exn ctx max_cost class_ =
    let low_cost_paths =
      H.find_exn ctx.paths class_
      |> List.filter ~f:(fun (p : Path.t) -> Path.cost p <= max_cost)
    in
    let p = List.random_element_exn low_cost_paths in
    Apply (p.op, List.map p.args ~f:(random_program_exn ctx (Path.cost p - 1)))

  let random_program_exn ?(max_cost = Int.max_value) ctx state =
    random_program_exn ctx max_cost state

  let clear { classes; paths } =
    H.clear classes;
    H.clear paths

  let validate ss eval dist thresh =
    Hashtbl.iteri ss.paths ~f:(fun ~key ~data ->
        List.iter data ~f:(fun (path : Path.t) ->
            let value = Path.value path in
            [%test_result: Lang.Value.t] ~message:"cached operator output" ~expect:value
              (eval path.op @@ List.map ~f:Class.value path.args);
            let d = dist (Class.value key) value in
            if d >. thresh then
              raise_s [%message "grouping not within threshold" (d : float)]))

  let rec local_greedy ss max_height
      (eval : Lang.Op.t -> Lang.Value.t list -> Lang.Value.t)
      (dist : Lang.Value.t -> float) (class_ : Class.t) =
    let open Option.Let_syntax in
    let all_paths = H.find_exn ss.paths class_ in

    let eligible_paths =
      List.filter ~f:(fun (p : Path.t) -> Path.height p <= max_height) all_paths
    in
    let n_sample = max 1 (List.length eligible_paths / 2) in
    let%bind _, best_path =
      Iter.of_list eligible_paths |> Iter.sample n_sample |> Iter.of_array
      |> Iter.map (fun (p : Path.t) -> (dist (Path.value p), p))
      |> Iter.min ~lt:(fun (d, _) (d', _) -> Float.(d < d'))
    in
    let best_path_arg_values = List.map best_path.args ~f:Class.value in

    let rec select_args (arg_values : Lang.Value.t list) i = function
      | [] -> Some []
      | c :: cs ->
          let dist' v = dist (eval best_path.op @@ List.set arg_values i v) in
          let%bind arg_prog = local_greedy ss (max_height - 1) eval dist' c in
          let arg_values' = List.set arg_values i @@ Program.eval eval arg_prog in
          let%bind rest_progs = select_args arg_values' (i + 1) cs in
          return (arg_prog :: rest_progs)
    in

    let%bind arg_progs = select_args best_path_arg_values 0 best_path.args in
    let prog = Apply (best_path.op, arg_progs) in
    return prog

  let rec centroid ss max_height (class_ : Class.t) =
    let open Option.Let_syntax in
    let%bind path =
      List.find (H.find_exn ss.paths class_) ~f:(fun (p : Path.t) ->
          Path.height p <= max_height
          && [%equal: Lang.Value.t] (Class.value class_) (Path.value p))
    in
    let%map args = List.map path.args ~f:(centroid ss (max_height - 1)) |> Option.all in
    Apply (path.op, args)

  let rec random ss max_height (class_ : Class.t) =
    let open Option.Let_syntax in
    let%bind best_path =
      H.find_exn ss.paths class_
      |> List.filter ~f:(fun (p : Path.t) -> Path.height p <= max_height)
      |> Iter.of_list |> Iter.sample 1 |> Array.random_element
    in
    let rec select_args i = function
      | [] -> Some []
      | c :: cs ->
          let%bind arg_prog = random ss (max_height - 1) c in
          let%bind rest_progs = select_args (i + 1) cs in
          return (arg_prog :: rest_progs)
    in
    let%bind arg_progs = select_args 0 best_path.args in
    let prog = Apply (best_path.op, arg_progs) in
    return prog

  let rec exhaustive ?(width = 4) ss max_height
      (eval : Lang.Op.t -> Lang.Value.t list -> Lang.Value.t)
      (dist : Lang.Value.t -> float) (class_ : Class.t) =
    let all_paths = H.find_exn ss.paths class_ in

    List.filter ~f:(fun (p : Path.t) -> Path.height p <= max_height) all_paths
    |> Iter.of_list
    |> Iter.map (fun (p : Path.t) -> (dist (Path.value p), p))
    |> Iter.sort ~cmp:(fun (d, _) (d', _) -> [%compare: float] d d')
    |> Iter.take width
    |> Iter.filter_map (fun (_, p) ->
           let open Option.Let_syntax in
           let arg_values = List.map p.args ~f:Class.value in

           let rec select_args (arg_values : Lang.Value.t list) i = function
             | [] -> Some []
             | c :: cs ->
                 let dist' v = dist (eval p.op @@ List.set arg_values i v) in
                 let%bind arg_prog = exhaustive ss (max_height - 1) eval dist' c in
                 let arg_values' = List.set arg_values i @@ Program.eval eval arg_prog in
                 let%bind rest_progs = select_args arg_values' (i + 1) cs in
                 return (arg_prog :: rest_progs)
           in

           let%bind arg_progs = select_args arg_values 0 p.args in
           let prog = Apply (p.op, arg_progs) in
           let d = dist (Program.eval eval prog) in
           return (d, prog))
    |> Iter.min ~lt:(fun (d, _) (d', _) -> d <. d')
    |> Option.map ~f:(fun (_, p) -> p)

  let all_paths ss = Iter.of_hashtbl_data ss.paths |> Iter.map Iter.of_list |> Iter.concat

  let in_paths ss c =
    Hashtbl.find ss.paths c |> Option.map ~f:Iter.of_list
    |> Option.value ~default:Iter.empty
end
