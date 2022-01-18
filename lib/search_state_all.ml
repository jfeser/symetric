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

  module Class = struct
    module T = struct
      type t = { type_ : Type.t; value : Value.t } [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)

    let[@inline] create v t = { type_ = t; value = v }
    let[@inline] value c = c.value
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

    let[@inline] create value op args = { value; op; args; cost = -1; height = -1 }

    let pp =
      let open Fmt.Dump in
      record [ field "op" (fun x -> x.op) Op.pp ]
  end

  type paths = { paths : Path0.t list; min_cost : int; min_height : int }
  [@@deriving sexp]

  type t = { classes : Class.t list H.M(Attr).t; paths : paths H.M(Class).t }
  [@@deriving sexp]

  module Path : sig
    type ctx
    type t = Path0.t [@@deriving sexp]

    val create : Value.t -> Op.t -> Class.t list -> t
    val cost : ctx -> t -> int
    val height : ctx -> t -> int
  end
  with type ctx := t = struct
    include Path0

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
          List.fold ~init:edges
            ~f:(fun edges (path : Path.t) ->
              let edge =
                (List.map path.args ~f:(H.find_exn node_ids), H.find_exn node_ids key)
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

  let search_iter ctx ~cost ~type_ =
    match H.find ctx.classes @@ Attr.create cost type_ with
    | Some q -> Iter.of_list q |> Iter.map (fun c -> c.Class.value)
    | None -> Iter.empty

  let search ctx ~cost ~type_ = search_iter ctx ~cost ~type_ |> Iter.to_list

  let rec find_term ctx = function
    | Apply (op, []) ->
        let all_values =
          H.to_alist ctx.paths
          |> List.filter_map ~f:(fun (v, paths) ->
                 if
                   List.exists
                     ~f:(function
                       | { op = op'; args = []; _ } -> [%compare.equal: Op.t] op op'
                       | _ -> false)
                     paths.paths
                 then Some v
                 else None)
        in
        print_s [%message (op : Op.t) (List.length all_values : int)];

        Apply ((op, all_values), [])
    | Apply (op, args) ->
        let args = List.map args ~f:(find_term ctx) in
        let arg_sets =
          List.map args ~f:(fun (Apply ((_, vs), _)) -> Set.of_list (module Class) vs)
        in
        let all_outputs =
          H.to_alist ctx.paths
          |> List.filter_map ~f:(fun (v, paths) ->
                 if
                   List.exists
                     ~f:(function
                       | { op = op'; args; _ } ->
                           [%compare.equal: Op.t] op op'
                           && List.for_all2_exn arg_sets args ~f:Set.mem)
                     paths.paths
                 then Some v
                 else None)
        in
        Apply ((op, all_outputs), args)

  let mem_class ctx class_ = H.mem ctx.paths class_

  let classes ctx ?cost ~type_ =
    match cost with
    | Some cost ->
        H.find ctx.classes @@ Attr.create cost type_
        |> Option.map ~f:Iter.of_list
        |> Option.value ~default:Iter.empty
    | None ->
        fun k ->
          H.iteri ctx.classes ~f:(fun ~key ~data ->
              if [%compare.equal: Type.t] key.type_ type_ then List.iter data ~f:k)

  let insert_class_members ctx class_ members =
    let new_paths =
      List.map members ~f:(fun (value, op, args) -> Path.create value op args)
    in
    H.update ctx.paths class_ ~f:(function
      | Some ps -> { ps with paths = new_paths @ ps.paths }
      | None -> failwith "class not present in search space")

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
    H.update ctx.classes (Attr.create cost class_.type_) ~f:(function
      | Some cs -> class_ :: cs
      | None -> [ class_ ]);

    let new_path = Path.create class_.value op args in
    H.add_exn ctx.paths ~key:class_
      ~data:{ min_cost = cost; min_height = height; paths = [ new_path ] }

  let length ctx = H.length ctx.paths

  let print_stats ctx =
    H.to_alist ctx.classes
    |> List.sort ~compare:(fun (a, _) (a', _) ->
           [%compare: int * Type.t] (a.cost, a.type_) (a'.cost, a'.type_))
    |> List.iter ~f:(fun (attr, classes) ->
           eprint_s
             [%message
               (attr.cost : int) (attr.type_ : Type.t) (List.length classes : int)]);
    Fmt.epr "Total: %d\n%!" (length ctx)

  let rec program_exn ctx max_height class_ =
    let p =
      List.find_exn
        ~f:(fun p -> Path.height ctx p <= max_height)
        (H.find_exn ctx.paths class_).paths
    in
    program_of_op_args_exn ctx max_height p.op p.args

  and program_of_op_args_exn ctx max_height op args =
    Apply (op, List.map args ~f:(program_exn ctx (max_height - 1)))

  let rec random_program_exn ctx max_cost class_ =
    let p =
      (H.find_exn ctx.paths class_).paths
      |> List.filter ~f:(fun p -> p.cost <= max_cost)
      |> List.random_element_exn
    in
    Apply (p.op, List.map p.args ~f:(random_program_exn ctx (Path.cost ctx p - 1)))

  let random_program_exn ?(max_cost = Int.max_value) ctx state =
    random_program_exn ctx max_cost state

  let clear { classes; paths } =
    H.clear classes;
    H.clear paths

  let list_set l i x = List.mapi l ~f:(fun j y -> if i = j then x else y)

  let validate ss eval dist thresh =
    Hashtbl.iteri ss.paths ~f:(fun ~key ~data ->
        List.iter data.paths ~f:(fun path ->
            [%test_result: Value.t] ~message:"cached operator output" ~expect:path.value
              (eval path.op @@ List.map ~f:Class.value path.args);
            let d = dist key.value path.value in
            if d >. thresh then
              raise_s [%message "grouping not within threshold" (d : float)]))

  let rec local_greedy value_pp ss max_height eval dist (class_ : Class.t) =
    let open Option.Let_syntax in
    let all_paths = (H.find_exn ss.paths class_).paths in
    let eligible_paths =
      Iter.of_list all_paths
      |> Iter.filter (fun (p : Path.t) -> Path.height ss p <= max_height)
      |> Iter.to_list
    in
    let n_sample = max 1 (List.length eligible_paths / 2) in
    let%bind _, best_path =
      Iter.of_list eligible_paths |> Iter.sample n_sample |> Iter.of_array
      |> Iter.map (fun (p : Path.t) -> (dist p.value, p))
      |> Iter.min ~lt:(fun (d, _) (d', _) -> Float.(d < d'))
    in
    let best_path_arg_values = List.map best_path.args ~f:(fun c -> c.value) in
    List.mapi best_path.args ~f:(fun i arg_class ->
        let dist' v = dist (eval best_path.op @@ list_set best_path_arg_values i v) in
        local_greedy value_pp ss (max_height - 1) eval dist' arg_class)
    |> Option.all
    |> Option.map ~f:(fun arg_progs -> Apply (best_path.op, arg_progs))
end
