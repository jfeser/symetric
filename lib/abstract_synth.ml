open Std

let debug = true

module type Lang_intf = sig
  module Type : sig
    type t [@@deriving compare, equal, hash, sexp]

    val default : t
    val output : t
  end

  module Op : sig
    type t [@@deriving compare, equal, hash, sexp]

    val default : t
    val pp : t Fmt.t
    val cost : t -> int
    val arity : t -> int
    val args_type : t -> Type.t list
    val ret_type : t -> Type.t
    val is_commutative : t -> bool
  end

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp]

    val default : t
    val eval : Op.t -> t list -> t
    val is_error : t -> bool
    val pp : t Fmt.t
  end
end

module type Domain_pred_intf = sig
  type concrete
  type op
  type t [@@deriving compare, equal, hash, sexp]

  val pp : t Fmt.t
  val cost : [ `Concrete of concrete | `Pred of t ] -> int
  val lift : concrete -> t Iter.t

  val implies :
    [ `Concrete of concrete | `Preds of t list ] ->
    [ `Concrete of concrete | `Preds of t list ] ->
    bool

  val eval : t -> concrete -> bool

  val transfer :
    op ->
    [ `Concrete of concrete | `Preds of t list ] list ->
    bool * concrete option * t list
end

module Make
    (Lang : Lang_intf)
    (Domain_pred : Domain_pred_intf
                     with type concrete := Lang.Value.t
                      and type op := Lang.Op.t) =
struct
  open Lang

  module Pred = struct
    module T = struct
      type t = [ `Concrete of Value.t | `Pred of Domain_pred.t ]
      [@@deriving compare, equal, hash, sexp]
    end

    include T
    include Comparator.Make (T)

    let cost = Domain_pred.cost

    let pp fmt = function
      | `Concrete v -> Value.pp fmt v
      | `Pred p -> Domain_pred.pp fmt p
  end

  module Abs_value = struct
    open Lang

    module T = struct
      type t = Bottom | Preds of Set.M(Pred).t [@@deriving compare, equal, hash, sexp]

      let default = Bottom

      let pp fmt = function
        | Bottom -> Fmt.pf fmt "⊥"
        | Preds ps ->
            let iter f x = Set.iter ~f x in
            (Fmt.brackets (Fmt.iter ~sep:Fmt.semi iter Pred.pp)) fmt ps

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
            Iter.cons `True
            @@ Iter.map (function #Pred.t as p -> p)
            @@ (Iter.of_set ps :> [> Pred.t ] Iter.t)

      let lift conc =
        let domain = Domain_pred.lift conc |> Iter.map (fun p -> `Pred p) in
        of_iter (Iter.cons (`Concrete conc) domain)

      let and_ p p' =
        match (p, p') with
        | Bottom, _ | _, Bottom -> Bottom
        | Preds ps, Preds ps' -> Preds (Set.union ps ps')

      let and_many = Iter.fold and_ true_

      let pack ps =
        let concrete =
          Iter.of_set ps |> Iter.find (function `Concrete _ as c -> Some c | _ -> None)
        in
        match concrete with
        | Some c -> c
        | None ->
            `Preds
              (Iter.of_set ps
              |> Iter.map (function `Pred p -> p | `Concrete _ -> assert false)
              |> Iter.to_list)

      let implies p p' =
        match (p, p') with
        | Bottom, _ -> true
        | _, Bottom -> false
        | Preds ps, Preds ps' -> Domain_pred.implies (pack ps) (pack ps')

      let contains p v =
        match p with
        | Bottom -> false
        | Preds ps ->
            Set.for_all ps ~f:(function
              | `Pred p -> Domain_pred.eval p v
              | `Concrete v' -> [%equal: Value.t] v v')

      let length = function Bottom -> 0 | Preds ps -> Set.length ps
      let singleton p = of_iter @@ Iter.singleton p
    end

    include T
    include Comparator.Make (T)

    module Ctx = struct
      type nonrec t = {
        preds : Set.M(Pred).t;
        max_conjuncts : int;
        refine_log :
          ((Op.t * Value.t * t) Program.t * Set.M(Pred).t * Set.M(Pred).t) Queue.t;
      }
      [@@deriving compare, equal, sexp]

      let create ?(max_conjuncts = 1) () =
        { preds = Set.empty (module Pred); max_conjuncts; refine_log = Queue.create () }
    end

    (** Implements the abstract transfer function for conjunctions of atomic predicates. *)
    let eval op args =
      let transfer = Domain_pred.transfer op in
      if List.exists args ~f:(function Bottom -> true | _ -> false) then Bottom
      else
        let args' =
          List.map args ~f:(function Bottom -> assert false | Preds ps -> pack ps)
        in
        let is_bot, concrete, preds = transfer args' in
        let ps = Option.map concrete ~f:(fun c -> `Concrete c) |> Option.to_list in
        let ps = List.map preds ~f:(fun p -> `Pred p) @ ps in
        if is_bot then Bottom else of_iter @@ Iter.of_list ps

    (** Restricts the abstract transfer to only use the predicates that we have
       discovered through refinement. *)
    let eval_restricted preds op args =
      match eval op args with Bottom as p -> p | Preds ps -> Preds (Set.inter preds ps)

    let conjuncts ~k l =
      let k = min k (List.length l) in
      let ret =
        Iter.(0 -- k)
        |> Iter.map (fun k ->
               if k = 0 then Iter.empty
               else Iter.map Array.to_list @@ Combinat.combinations ~k l)
        |> Iter.concat
        |> Iter.map (fun cs ->
               (List.sum (module Int) cs ~f:Pred.cost, of_iter @@ Iter.of_list cs))
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

    let strengthen_enum ~k too_strong too_weak check =
      let n_args = List.length too_strong in
      assert (List.length too_weak = n_args);

      let module Conjunct_lang = struct
        module Type = struct
          type t = Args | Pred of int [@@deriving compare, equal, hash, sexp]

          let output = Args
          let default = Args
        end

        module Op = struct
          type t = Args | And of int | Pred of int * [ Pred.t | `True ]
          [@@deriving compare, equal, hash, sexp]

          let default = Args
          let pp = Fmt.nop
          let arity = function Args -> n_args | And _ -> 2 | Pred _ -> 0

          let ret_type : _ -> Type.t = function
            | Args -> Args
            | And slot -> Pred slot
            | Pred (slot, _) -> Pred slot

          let args_type = function
            | Args -> List.init n_args ~f:(fun slot -> Type.Pred slot)
            | And slot -> [ Type.Pred slot; Type.Pred slot ]
            | Pred _ -> []

          let is_commutative _ = false

          let cost = function
            | Pred (_, `True) -> 1
            | Pred (_, (#Pred.t as p)) -> Pred.cost p
            | Args | And _ -> 1
        end

        module Value = struct
          type nonrec t = Args of t list | Pred of t
          [@@deriving compare, equal, hash, sexp]

          let default = Pred Bottom

          let eval op args =
            match (op, args) with
            | Op.Args, preds ->
                Args (List.map preds ~f:(function Pred p -> p | _ -> assert false))
            | And _, [ Pred p; Pred p' ] -> Pred (and_ p p')
            | Pred (_, p), [] -> Pred (singleton p)
            | _ -> assert false

          let is_error = function Pred p -> length p > k | _ -> false
        end

        let operators =
          Op.Args
          :: List.concat_mapi too_strong ~f:(fun slot conc ->
                 let pred_ops =
                   lift conc |> to_iter
                   |> Iter.map (fun p -> Op.Pred (slot, p))
                   |> Iter.to_list
                 in
                 Op.And slot :: pred_ops)
      end in
      let open Conjunct_lang in
      let exception Done of t list in
      let goal =
        `Pred
          (fun _ v ->
            match v with
            | Value.Args args ->
                let args = List.map2_exn args too_weak ~f:and_ in
                if check args then raise (Done args) else false
            | _ -> false)
      in
      let params = Baseline.Params.create ~max_cost:Int.max_value () in
      Baseline.synthesize (module Conjunct_lang) params goal
      |> Option.map ~f:(fun program ->
             match Program.eval Value.eval program with
             | Value.Args args -> args
             | _ -> assert false)
      |> Option.value_exn ~message:"failed to refine"

    let strengthen_simple ~k too_strong too_weak implies =
      let candidates =
        List.map too_strong ~f:lift
        |> List.map ~f:(fun p ->
               to_iter p
               |> Iter.filter_map (function #Pred.t as p -> Some p | _ -> None)
               |> Iter.to_list)
        |> per_arg_conjuncts ~k
        |> Iter.sort ~cmp:(fun (c, _) (c', _) -> [%compare: int] c c')
        |> Iter.to_list
      in
      let candidates =
        List.map ~f:(fun (_, cs) -> List.map2_exn cs too_weak ~f:and_) candidates
      in
      List.find_exn candidates ~f:implies

    let strengthen = strengthen_enum

    let strengthen_root (ctx : Ctx.t) too_strong too_weak contains_target =
      if debug then
        Fmt.epr "@[<v>Strengthening program root.@,Too strong: %a@,Too weak: %a@,%!@]"
          Value.pp too_strong pp too_weak;
      let new_pred =
        match
          strengthen ~k:ctx.max_conjuncts [ too_strong ] [ too_weak ] (function
            | [ p ] -> not (contains_target p)
            | _ -> assert false)
        with
        | [ p ] -> p
        | _ -> assert false
      in
      new_pred

    let rec strengthen_program (ctx : Ctx.t) (Program.Apply ((op, conc, abs), args)) out =
      let args' =
        if List.is_empty args then []
        else
          let args_conc, args_abs =
            List.map args ~f:(fun (Program.Apply ((_, c, a), _)) -> (c, a)) |> List.unzip
          in
          let out' =
            strengthen ~k:ctx.max_conjuncts args_conc args_abs (fun cs ->
                implies (eval op cs) out)
          in
          List.map2_exn args out' ~f:(strengthen_program ctx)
      in
      Program.Apply ((op, conc, abs, out), args')

    let refine (ctx : Ctx.t) contains_target p : Ctx.t option =
      (* Annotate program with concrete & restricted abstract values. *)
      let rec eval_all (ctx : Ctx.t) (Program.Apply (op, args)) =
        let args' = List.map args ~f:(eval_all ctx) in
        let args_conc, args_abs =
          List.map args' ~f:(fun (Program.Apply ((_, c, a), _)) -> (c, a)) |> List.unzip
        in
        let conc = Value.eval op args_conc
        and abs = eval_restricted ctx.preds op args_abs in
        Apply ((op, conc, abs), args')
      in
      let p = eval_all ctx p in

      let (Apply ((_, conc, abs), _)) = p in
      let p' = strengthen_program ctx p (strengthen_root ctx conc abs contains_target) in
      let preds =
        Program.iter p'
        |> Iter.map (fun ((_, _, _, ps), _) -> to_iter ps)
        |> Iter.concat
        |> Iter.filter_map (function `True -> None | #Pred.t as p -> Some p)
        |> Iter.to_set (module Pred)
      in
      (if debug then
         let new_preds = Set.diff preds ctx.preds in
         Fmt.epr "@[<v>Added new predicates: %a\n%!@]" pp (Preds new_preds));
      let preds' = Set.union ctx.preds preds in

      Queue.enqueue ctx.refine_log (p, ctx.preds, preds');

      Some { ctx with preds = preds' }

    let is_error = function Bottom -> true | _ -> false
  end

  let params = Baseline.Params.create ~max_cost:Int.max_value ()

  let synth contains_target equals_target ops =
    let exception Done of int * Lang.Op.t Program.t in
    let rec loop iters (ctx : Abs_value.Ctx.t) =
      let module Abs = struct
        include Lang

        module Value = struct
          include Abs_value

          (* When searching, only consider the predicates in the context *)
          let eval = eval_restricted ctx.preds
        end

        let operators = ops
      end in
      let ctx' =
        let goal =
          `Pred
            (fun op s ->
              [%equal: Abs.Type.t] (Abs.Op.ret_type op) Abs.Type.output
              && contains_target s)
        in
        match Baseline.synthesize (module Abs) params goal with
        | Some p ->
            Fmt.epr "Found program: %a\n%!" (Program.pp Lang.Op.pp) p;
            let v = Program.eval Lang.Value.eval p in
            if equals_target v then raise @@ Done (iters, p)
            else (
              Fmt.epr "Refining...\n%!";
              Abs.Value.refine ctx contains_target p)
        | None -> failwith "synthesis failed"
      in
      match ctx' with
      | Some ctx' ->
          if [%equal: Abs.Value.Ctx.t] ctx ctx' then
            raise_s
              [%message
                "refinement failed" (ctx : Abs.Value.Ctx.t) (ctx' : Abs.Value.Ctx.t)];
          loop (iters + 1) ctx'
      | None -> failwith "refinement failed"
    in
    let ctx = Abs_value.Ctx.create () in
    (try ignore (loop 0 ctx : Abs_value.Ctx.t)
     with Done (iters, p) ->
       eprint_s
         [%message
           "synthesis completed" (iters : int) (Program.size p : int) (p : Op.t Program.t)]);
    ctx.refine_log

  let synth_simple target ops =
    synth (fun a -> Abs_value.contains a target) ([%equal: Lang.Value.t] target) ops
end
