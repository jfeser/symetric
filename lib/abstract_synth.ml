open Std

let debug = true

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

    module Ctx : sig
      type t
    end

    val eval : Ctx.t -> Op.t -> t list -> t

    val is_error : t -> bool
  end
end

module type Domain_pred_intf = sig
  type concrete

  type op

  type ctx

  type t [@@deriving compare, hash, sexp]

  val cost : [ `Concrete of concrete | `Pred of t ] -> int

  val lift : concrete -> t Iter.t

  val complete : t -> t Iter.t

  val eval : t -> concrete -> bool

  val transfer :
    ctx ->
    op ->
    [ `True | `Concrete of concrete | `Pred of t ] list ->
    [ `False | `Concrete of concrete | `Pred of t ] list
end

module Make
    (Lang : Lang_intf)
    (Domain_pred : Domain_pred_intf
                     with type concrete := Lang.Value.t
                      and type op := Lang.Op.t
                      and type ctx := Lang.Value.Ctx.t) =
struct
  module Abs_value = struct
    open Lang

    module Pred = struct
      module T = struct
        type t = [ `Concrete of Value.t | `Pred of Domain_pred.t ] [@@deriving compare, hash, sexp]
      end

      include T
      include Comparator.Make (T)

      let cost = Domain_pred.cost
    end

    module T = struct
      type t = Bottom | Preds of Set.M(Pred).t [@@deriving compare, equal, hash, sexp]

      let true_ = Preds (Set.empty (module Pred))

      let of_iter iter =
        Iter.fold
          (fun ps p ->
            match (ps, p) with
            | ps, `True -> ps
            | _, `False | Bottom, _ -> Bottom
            | Preds pp, ((`Pred _ | `Concrete _) as p) -> Preds (Set.add pp p))
          true_ iter

      let to_iter = function
        | Bottom -> Iter.empty
        | Preds ps ->
            Iter.cons `True @@ Iter.map (function #Pred.t as p -> p) @@ (Iter.of_set ps :> [> Pred.t ] Iter.t)

      let lift conc =
        let domain = Domain_pred.lift conc |> Iter.map (fun p -> `Pred p) in
        of_iter (Iter.cons (`Concrete conc) domain)

      let and_ p p' =
        match (p, p') with Bottom, _ | _, Bottom -> Bottom | Preds ps, Preds ps' -> Preds (Set.union ps ps')

      let and_many = Iter.fold and_ true_

      let complete = function
        | Bottom as p -> p
        | Preds ps ->
            Iter.of_set ps
            |> Iter.map (function
                 | `Concrete v as p -> and_ (Preds (Set.singleton (module Pred) p)) @@ lift v
                 | `Pred p -> Domain_pred.complete p |> Iter.map (fun p -> `Pred p) |> of_iter)
            |> and_many

      let implies p p' =
        let p = complete p and p' = complete p' in
        match (p, p') with Bottom, _ -> true | _, Bottom -> false | Preds ps, Preds ps' -> Set.is_subset ps' ~of_:ps

      let contains p v =
        match p with
        | Bottom -> false
        | Preds ps ->
            Set.for_all ps ~f:(function
              | `Pred p -> Domain_pred.eval p v
              | `Concrete v' -> [%compare.equal: Value.t] v v')

      let length = function Bottom -> 0 | Preds ps -> Set.length ps

      let singleton p = of_iter @@ Iter.singleton p
    end

    include T
    include Comparator.Make (T)

    module Ctx = struct
      type nonrec t = {
        preds : Set.M(Pred).t;
        max_conjuncts : int;
        ectx : (Value.Ctx.t[@opaque]); [@compare.ignore]
        refine_log : ((Op.t * Value.t * t) Program.t * Set.M(Pred).t * Set.M(Pred).t) Queue.t;
      }
      [@@deriving compare, sexp]

      let create ?(max_conjuncts = 3) ectx =
        { preds = Set.empty (module Pred); max_conjuncts; ectx; refine_log = Queue.create () }
    end

    (** Implements the abstract transfer function for conjunctions of atomic predicates. *)
    let eval (ctx : Ctx.t) op args =
      let transfer = Domain_pred.transfer ctx.ectx op in
      List.map args ~f:to_iter |> Iter.list_product |> Iter.map transfer |> Iter.map Iter.of_list |> Iter.concat
      |> of_iter

    (** Restricts the abstract transfer to only use the predicates that we have
       discovered through refinement. *)
    let eval_restricted ctx op args =
      match eval ctx op args with Bottom as p -> p | Preds ps -> Preds (Set.inter ctx.preds ps)

    let conjuncts ~k l =
      let k = min k (List.length l) in
      let ret =
        Iter.(0 -- k)
        |> Iter.map (fun k -> if k = 0 then Iter.empty else Iter.map Array.to_list @@ Combinat.combinations ~k l)
        |> Iter.concat
        |> Iter.map (fun cs -> (List.sum (module Int) cs ~f:Pred.cost, of_iter @@ Iter.of_list cs))
      in
      ret

    let per_arg_conjuncts ~k args =
      let rec per_arg_conjuncts = function
        | [] -> Iter.empty
        | [ x ] -> conjuncts ~k x |> Iter.map (fun (c, x) -> (c, [ x ]))
        | x :: xs ->
            Iter.product (conjuncts ~k x) (per_arg_conjuncts xs)
            |> Iter.map (fun ((c, x), (c', xs')) -> (c + c', x :: xs'))
      in
      per_arg_conjuncts args

    let _strengthen ~k too_strong too_weak check =
      let n_args = List.length too_strong in
      assert (List.length too_weak = n_args);

      print_s [%message (too_strong : Value.t list) (too_weak : t list)];

      let module Conjunct_lang = struct
        module Type = struct
          type t = Args | Pred of int [@@deriving compare, hash, sexp]

          let output = Args
        end

        module Op = struct
          type t = Args | And of int | Pred of int * [ Pred.t | `True ] [@@deriving compare, hash, sexp]

          let arity = function Args -> n_args | And _ -> 2 | Pred _ -> 0

          let ret_type : _ -> Type.t = function Args -> Args | And slot -> Pred slot | Pred (slot, _) -> Pred slot

          let args_type = function
            | Args -> List.init n_args ~f:(fun slot -> Type.Pred slot)
            | And slot -> [ Type.Pred slot; Type.Pred slot ]
            | Pred _ -> []

          let cost = function Pred (_, `True) -> 1 | Pred (_, (#Pred.t as p)) -> Pred.cost p | Args | And _ -> 1
        end

        module Value = struct
          type nonrec t = Args of t list | Pred of t [@@deriving compare, hash, sexp]

          module Ctx = Unit

          let eval _ op args =
            match (op, args) with
            | Op.Args, preds -> Args (List.map preds ~f:(function Pred p -> p | _ -> assert false))
            | And _, [ Pred p; Pred p' ] -> Pred (and_ p p')
            | Pred (_, p), [] -> Pred (singleton p)
            | _ -> assert false

          let is_error = function Pred p -> length p > k | _ -> false
        end
      end in
      let open Conjunct_lang in
      let module Synth = Baseline.Make (Conjunct_lang) in
      let ops =
        Op.Args
        :: List.concat_mapi too_strong ~f:(fun slot conc ->
               let pred_ops = lift conc |> to_iter |> Iter.map (fun p -> Op.Pred (slot, p)) |> Iter.to_list in
               Op.And slot :: pred_ops)
      in
      print_s [%message (ops : Op.t list)];
      let exception Done of t list in
      let synth =
        let config =
          Synth.Ctx.create () ops
            (`Pred
              (fun _ v ->
                match v with
                | Value.Args args ->
                    let args = List.map2_exn args too_weak ~f:and_ in
                    print_s [%message (args : t list)];
                    if check args then raise (Done args) else false
                | _ -> false))
        in
        new Synth.synthesizer config
      in
      try
        let (_ : _) = synth#run in
        raise_s [%message "failed to refine"]
      with Done args -> args

    (* let strengthen ~k too_strong too_weak implies = *)
    (*   let candidates = *)
    (*     List.map too_strong ~f:lift *)
    (*     |> List.map ~f:(fun p -> *)
    (*            to_iter p |> Iter.filter_map (function #Pred.t as p -> Some p | _ -> None) |> Iter.to_list) *)
    (*     |> per_arg_conjuncts ~k *)
    (*     |> Iter.sort ~cmp:(fun (c, _) (c', _) -> [%compare: int] c c') *)
    (*     |> Iter.to_list *)
    (*   in *)
    (*   if debug then *)
    (*     eprint_s [%message (too_strong : Value.t list) (too_weak : t list) (candidates : (int * t list) list)]; *)
    (*   let candidates = List.map ~f:(fun (_, cs) -> List.map2_exn cs too_weak ~f:and_) candidates in *)
    (*   List.find_exn candidates ~f:implies *)

    let strengthen = _strengthen

    let strengthen_root (ctx : Ctx.t) too_strong too_weak target =
      if debug then eprint_s [%message "strengthening program root" (too_strong : Value.t) (too_weak : t)];
      let new_pred =
        match
          strengthen ~k:ctx.max_conjuncts [ too_strong ] [ too_weak ] (function
            | [ p ] -> not (contains p target)
            | _ -> assert false)
        with
        | [ p ] -> p
        | _ -> assert false
      in
      new_pred

    let rec strengthen_program (ctx : Ctx.t) (Program.Apply ((op, conc, abs), args) as p) out =
      if debug then eprint_s [%message "strengthening program" (p : (Op.t * Value.t * t) Program.t) (out : t)];
      let args' =
        if List.is_empty args then []
        else
          let args_conc, args_abs = List.map args ~f:(fun (Program.Apply ((_, c, a), _)) -> (c, a)) |> List.unzip in
          let out' = strengthen ~k:ctx.max_conjuncts args_conc args_abs (fun cs -> implies (eval ctx op cs) out) in
          List.map2_exn args out' ~f:(strengthen_program ctx)
      in
      Program.Apply ((op, conc, abs, out), args')

    let refine (ctx : Ctx.t) (target : Value.t) p : Ctx.t option =
      (* Annotate program with concrete & restricted abstract values. *)
      let rec eval_all (ctx : Ctx.t) (Program.Apply (op, args)) =
        let args' = List.map args ~f:(eval_all ctx) in
        let args_conc, args_abs = List.map args' ~f:(fun (Program.Apply ((_, c, a), _)) -> (c, a)) |> List.unzip in
        let conc = Value.eval ctx.ectx op args_conc and abs = eval_restricted ctx op args_abs in
        Apply ((op, conc, abs), args')
      in
      let p = eval_all ctx p in

      let (Apply ((_, conc, abs), _)) = p in
      let p' = strengthen_program ctx p (strengthen_root ctx conc abs target) in
      let preds =
        Program.ops_iter p'
        |> Iter.map (fun (_, _, _, ps) -> to_iter ps)
        |> Iter.concat
        |> Iter.filter_map (function `True -> None | #Pred.t as p -> Some p)
        |> Iter.to_set (module Pred)
      in
      let preds' = Set.union ctx.preds preds in

      Queue.enqueue ctx.refine_log (p, ctx.preds, preds');

      Some { ctx with preds = preds' }

    let is_error = function Bottom -> true | _ -> false
  end

  let synth ectx target ops =
    let module Abs = struct
      include Lang

      module Value = struct
        include Abs_value

        (* When searching, only consider the predicates in the context *)
        let eval = eval_restricted
      end
    end in
    let exception Done of int * Abs.Op.t Program.t in
    let module Synth = Baseline.Make (Abs) in
    let rec loop iters ctx =
      let ctx' =
        let sctx =
          Synth.Ctx.create ctx ops
          @@ `Pred
               (fun op s ->
                 [%compare.equal: Abs.Type.t] (Abs.Op.ret_type op) Abs.Type.output && Abs.Value.contains s target)
        in
        let synth = new Synth.synthesizer sctx in
        match synth#run with
        | Some p ->
            let v = Program.eval (Lang.Value.eval ctx.ectx) p in
            if [%compare.equal: Lang.Value.t] v target then (
              eprint_s
                [%message
                  (Synth.Search_state.n_states synth#get_search_state : int)
                    (Synth.Search_state.n_transitions synth#get_search_state : int)];
              Synth.Search_state.print_stats synth#get_search_state;
              raise @@ Done (iters, p))
            else Abs.Value.refine ctx target p
        | None -> failwith "synthesis failed"
      in
      match ctx' with
      | Some ctx' ->
          if [%compare.equal: Abs.Value.Ctx.t] ctx ctx' then
            raise_s [%message "refinement failed" (ctx : Abs.Value.Ctx.t) (ctx' : Abs.Value.Ctx.t)];
          loop (iters + 1) ctx'
      | None -> failwith "refinement failed"
    in
    let ctx = Abs.Value.Ctx.create ectx in
    (try ignore (loop 0 ctx : Abs_value.Ctx.t)
     with Done (iters, p) ->
       eprint_s [%message "synthesis completed" (iters : int) (Program.size p : int) (p : Abs.Op.t Program.t)]);
    ctx.refine_log
end
