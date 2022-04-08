open Std

module type Subst_intf = sig
  type t [@@deriving sexp]
  type k
  type v

  val empty : t
  val set : t -> k -> v -> t
  val get : t -> k -> v option
end

module type Term_set_intf = sig
  type t [@@deriving compare, sexp]
  type op

  val heads : t -> (op * t list) Iter.t
end

module Pattern = struct
  type 'o t = Apply of ('o * 'o t list) | Var of int [@@deriving compare, sexp]

  let apply x y = Apply (x, y)
  let rec of_program Program.(Apply (op, args)) = Apply (op, List.map args ~f:of_program)

  let leaf_patterns (type op) (module Op : Op_intf.S with type t = op) ops =
    let leaf_ops = List.filter ops ~f:(fun op -> Op.arity op = 0) in
    List.concat_map leaf_ops ~f:(fun op1 ->
        List.filter_map leaf_ops ~f:(fun op2 ->
            Option.some_if
              (not ([%compare.equal: Op.t] op1 op2))
              (Apply (op1, []), Apply (op2, []))))

  let rename_patterns ?(max_arity = Int.max_value) (type op)
      (module Op : Op_intf.S with type t = op) ops =
    let ops = List.filter ops ~f:(fun op -> Op.arity op <= max_arity) in
    List.concat_map ops ~f:(fun op1 ->
        List.filter ops ~f:(fun op2 ->
            (not ([%compare.equal: Op.t] op1 op2)) && Op.arity op1 = Op.arity op2)
        |> List.map ~f:(fun op2 ->
               let args = List.init (Op.arity op1) ~f:(fun i -> Var i) in
               (Apply (op1, args), Apply (op2, args))))

  module type Comparable = sig
    type t [@@deriving compare]
  end

  let match_root (type op) (module Op : Comparable with type t = op) init bind p t =
    let bind ctx k v = Option.bind ctx ~f:(fun ctx -> bind ctx k v) in
    let rec match_ ctx p t =
      match (p, t) with
      | Var v, t -> bind ctx v t
      | Apply (op, args), Program.Apply (op', args') ->
          if [%compare.equal: Op.t] op op' then
            List.fold2_exn args args' ~init:ctx ~f:(fun ctx p' t' -> match_ ctx p' t')
          else None
    in
    match_ (Some init) p t

  module type Match_op = sig
    type t [@@deriving compare, sexp]
  end

  let match_ (type op ts subst) (module Op : Match_op with type t = op)
      (module T : Term_set_intf with type t = ts and type op = Op.t)
      (module S : Subst_intf with type t = subst and type k = int and type v = T.t) p ts k
      =
    let rec match_ ctx jobs =
      (* print_s [%message (ctx : S.t) (jobs : (Op.t t * T.t) list)]; *)
      match jobs with
      | [] -> k ctx
      | (Var v, t) :: jobs -> (
          match S.get ctx v with
          | Some t' -> if [%compare.equal: T.t] t t' then match_ ctx jobs
          | None ->
              let ctx' = S.set ctx v t in
              match_ ctx' jobs)
      | (Apply (op, args), t) :: jobs ->
          T.heads t
          |> Iter.iter (fun (op', args') ->
                 if [%compare.equal: Op.t] op op' then
                   let jobs' = List.zip_exn args args' in
                   match_ ctx (jobs' @ jobs))
    in
    match_ S.empty [ (p, ts) ]

  let rec match_all op_m init bind p t k =
    Option.iter (match_root op_m init bind p t) ~f:k;
    let (Apply (_, args)) = t in
    List.iter args ~f:(fun t' -> match_all op_m init bind p t' k)

  let rec subst subst_var = function
    | Var v -> subst_var v
    | Apply (op, args) -> Program.Apply (op, List.map args ~f:(subst subst_var))

  let rewrite_root (type op) ((module Op : Comparable with type t = op) as op) (lhs, rhs)
      t =
    Option.map
      (match_root op
         (Map.empty (module Int))
         (fun m k v ->
           match Map.find m k with
           | Some v' -> if [%compare.equal: Op.t Program.t] v v' then Some m else None
           | None -> Some (Map.add_exn m ~key:k ~data:v))
         lhs t)
      ~f:(fun ctx -> subst (Map.find_exn ctx) rhs)

  let rec rewrite_all op_m rule t k =
    Option.iter (rewrite_root op_m rule t) ~f:k;
    let (Apply (op, args)) = t in
    List.iteri args ~f:(fun i t' ->
        rewrite_all op_m rule t' (fun p ->
            k @@ Program.Apply (op, List.take args ~n:i @ (p :: List.drop args (i + 1)))))
end

module Rule = struct
  type 'o t = 'o Pattern.t * 'o Pattern.t [@@deriving compare, sexp]

  let flip (x, y) = (y, x)

  let normalize (type op) compare_op rules =
    List.concat_map rules ~f:(fun r -> [ r; flip r ])
    |> List.dedup_and_sort ~compare:[%compare: op t]
end

let of_rules ?n ?target ~dist m_op rules eval =
  let propose term =
    let sampler = Sample.Incremental.reservoir 1 in
    List.iter rules ~f:(fun ((lhs, rhs) as rule) ->
        Pattern.rewrite_all m_op rule term sampler.add;
        Pattern.rewrite_all m_op (rhs, lhs) term sampler.add);
    Option.value ~default:term @@ List.hd @@ sampler.get_sample ()
  in
  let score =
    match target with Some t -> fun p -> dist t @@ eval p | None -> Fun.const 1.0
  in
  let search center k = Sample.stochastic ?n ~propose ~score center k in
  search

let tabu (type state) ?(max_tabu = 10) ~neighbors state start k =
  let (module State : Base.Hashable.Key with type t = state) = state in
  let seen = Hash_queue.create @@ Base.Hashable.of_key state in
  let rec loop current =
    let m_next =
      Iter.find_pred (fun c -> not (Hash_queue.mem seen c)) (neighbors current)
    in
    match m_next with
    | Some next ->
        (* print_s [%message (current : State.t) (next : State.t)]; *)
        Hash_queue.enqueue_back_exn seen next ();
        if Hash_queue.length seen > max_tabu then Hash_queue.drop_front seen;
        k next;
        loop next
    | None -> ()
  in
  Hash_queue.enqueue_back_exn seen start ();
  loop start

let of_rules_root_only_tabu ~dist ~target m_op rules eval =
  let neighbors t =
    List.concat_map rules ~f:(fun rule ->
        (Option.to_list @@ Pattern.rewrite_root m_op rule t)
        @ Option.to_list
        @@ Pattern.rewrite_root m_op (Rule.flip rule) t)
    |> List.map ~f:(fun t' -> (dist (eval t') target, t'))
    |> List.sort ~compare:(fun (d, _) (d', _) -> [%compare: float] d d')
    |> List.map ~f:(fun (_, v) -> v)
    |> Iter.of_list
  in
  let module P = struct
    module T = struct
      type t = Cad_op.t Program.t [@@deriving compare, hash, sexp]
    end

    include T
  end in
  tabu ~neighbors (module P)

module type Value_intf = sig
  type t [@@deriving compare, hash, sexp]
end

let rewrite_all unnormalize t k =
  let rec rewrite_all t k =
    unnormalize t |> List.iter ~f:k;
    let (Program.Apply (op, args)) = t in
    List.iteri args ~f:(fun i t' ->
        rewrite_all t' (fun p ->
            k @@ Program.Apply (op, List.take args ~n:i @ (p :: List.drop args (i + 1)))))
  in
  rewrite_all t k

let of_unnormalize_tabu (type op value) ?(max_tabu = 1000) ~dist ~target
    (module Op : Value_intf with type t = op)
    (module Value : Value_intf with type t = value) unnormalize eval start =
  let module State = struct
    type t = { program : Op.t Program.t; value : Value.t }
    [@@deriving compare, hash, sexp]
  end in
  let neighbors (t : State.t) =
    let cmp = [%compare: float * _] in

    let choices =
      rewrite_all unnormalize t.program
      |> Iter.map (fun p ->
             let value = eval p in
             (-.dist value target, State.{ program = p; value }))
      |> Iter.to_list
    in

    Iter.of_list choices
    |> Iter.top_k ~compare:[%compare: float * _] (max_tabu + 1)
    |> Iter.map (fun (d, x) -> (-.d, x))
    |> Iter.sort ~cmp |> Iter.map Tuple.T2.get2
  in

  tabu ~max_tabu ~neighbors (module State) { program = start; value = eval start }
  |> Iter.map (fun s -> s.State.program)
