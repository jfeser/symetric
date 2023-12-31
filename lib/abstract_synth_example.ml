open Std

let debug = false

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

    module Ctx : sig
      type t
    end

    type example

    val default : t
    val eval : Ctx.t -> example -> Op.t -> t list -> t
    val is_error : t -> bool
    val pp : t Fmt.t
  end
end

module type Domain_pred_intf = sig
  type concrete
  type op
  type ctx
  type example
  type t [@@deriving compare, equal, hash, sexp]

  val pp : t Fmt.t
  val cost : [ `Concrete of concrete | `Pred of t ] -> int
  val lift : concrete -> [ `Pred of t | `False ] Iter.t

  val implies :
    [ `Concrete of concrete | `Pred of t | `True | `False ] list ->
    [ `Concrete of concrete | `Pred of t | `True | `False ] list ->
    bool

  val eval : t -> concrete -> bool

  val transfer :
    [ `Concrete of concrete | `Pred of t | `True | `False ] list ->
    ctx ->
    example ->
    op ->
    [ `Concrete of concrete | `Pred of t | `True | `False ] list ->
    [ `Concrete of concrete | `Pred of t | `True | `False ] list
end

module Make
    (Lang : Lang_intf)
    (Domain_pred : Domain_pred_intf
                     with type concrete := Lang.Value.t
                      and type op := Lang.Op.t
                      and type ctx := Lang.Value.Ctx.t
                      and type example = Lang.Value.example) =
struct
  module Abs_value = struct
    open Lang

    module Pred = struct
      module T = struct
        type t = [ `Concrete of Value.t | `Pred of Domain_pred.t | `True | `False ]
        [@@deriving compare, hash, sexp]
      end

      include T
      include Comparator.Make (T)

      let cost = function
        | `True | `False -> 1
        | (`Concrete _ | `Pred _) as p -> Domain_pred.cost p

      let pp fmt = function
        | `Concrete v -> Value.pp fmt v
        | `Pred p -> Domain_pred.pp fmt p
        | `True -> Fmt.pf fmt "true"
        | `False -> Fmt.pf fmt "false"
    end

    module T = struct
      type t = Set.M(Pred).t list [@@deriving compare, equal, hash, sexp]

      let default = []
      let pp fmt x = Iter.pp_seq Pred.pp fmt @@ Iter.of_set x
    end

    include T
    include Comparator.Make (T)

    module Ctx = struct
      type nonrec t = {
        preds : Set.M(Pred).t;
        max_conjuncts : int;
        ectx : (Value.Ctx.t[@opaque]); [@ignore]
      }
      [@@deriving compare, equal, sexp]

      let create ?(max_conjuncts = 3) ?(initial_preds = []) ectx =
        {
          preds = Set.of_list (module Pred) ([ `True; `False ] @ initial_preds);
          max_conjuncts;
          ectx;
        }
    end

    let eval_single (ctx : Ctx.t) example op args =
      List.map args ~f:(fun ps -> Iter.cons `True @@ Iter.of_set ps)
      |> Iter.list_product
      |> Iter.map (fun simple_args ->
             Domain_pred.transfer (Set.to_list ctx.preds) ctx.ectx example op simple_args
             |> Iter.of_list)
      |> Iter.concat
      |> Iter.fold Set.add (Set.empty (module Pred))

    (** Implements the abstract transfer function for conjunctions of atomic predicates. *)
    let eval (ctx : Ctx.t) examples op args =
      (* args = [ col1 ... coln ] where each col = [a1 ... ak] is an abstract
         value describing the behavior of the program on example k *)
      (* after transpose, args_t = [ [ a11 ... a1k ] ... ] contains one abstract argument list for each example *)
      match args with
      | [] -> List.map examples ~f:(fun example -> eval_single ctx example op [])
      | args ->
          let args_t = List.transpose_exn args in
          List.map2_exn examples args_t ~f:(fun example arg_v ->
              eval_single ctx example op arg_v)

    (** Restricts the abstract transfer to only use the predicates that we have
       discovered through refinement. *)
    let eval_restricted (ctx : Ctx.t) examples op args =
      let ret = eval ctx examples op args in
      let ret = List.map ret ~f:(Set.inter ctx.preds) in
      ret

    let eval_restricted_single (ctx : Ctx.t) example op args =
      eval_single ctx example op args |> Set.inter ctx.preds

    let conjuncts ~k (l : Pred.t list) =
      let k = min k (List.length l) in
      let ret =
        Iter.(0 -- k)
        |> Iter.map (fun k ->
               if k = 0 then Iter.empty
               else Iter.map Array.to_list @@ Combinat.combinations ~k l)
        |> Iter.concat
        |> Iter.map (fun cs ->
               ( List.sum (module Int) cs ~f:Pred.cost,
                 Set.of_list (module Pred) (cs :> Pred.t list) ))
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

    let strengthen_simple ~k too_strong too_weak implies =
      let candidates =
        List.map too_strong ~f:(fun c ->
            let (ps : Pred.t list) =
              (Iter.to_list @@ Domain_pred.lift c :> Pred.t list)
            in
            `Concrete c :: ps)
        |> per_arg_conjuncts ~k
        |> Iter.sort ~cmp:(fun (c, _) (c', _) -> [%compare: int] c c')
        |> Iter.to_list
      in
      let candidates =
        List.map ~f:(fun (_, cs) -> List.map2_exn cs too_weak ~f:Set.union) candidates
      in
      List.find_exn candidates ~f:implies

    let strengthen = strengthen_simple

    let strengthen_root (ctx : Ctx.t) too_strong too_weak contains_target : Set.M(Pred).t
        =
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

    let rec strengthen_program (ctx : Ctx.t) example
        (Program.Apply ((op, conc, abs), args)) (out : Set.M(Pred).t) : _ Program.t =
      let args' =
        if List.is_empty args then []
        else
          let args_conc, args_abs =
            List.map args ~f:(fun (Program.Apply ((_, c, a), _)) -> (c, a)) |> List.unzip
          in
          let out' =
            strengthen ~k:ctx.max_conjuncts args_conc args_abs (fun cs ->
                Domain_pred.implies
                  (Set.to_list @@ eval_single ctx example op cs)
                  (Set.to_list out))
          in
          List.map2_exn args out' ~f:(strengthen_program ctx example)
      in
      Program.Apply ((op, conc, abs, out), args')

    let refine (ctx : Ctx.t) example contains_target p : Ctx.t option =
      (* Annotate program with concrete & restricted abstract values. *)
      let rec eval_all (ctx : Ctx.t) (Program.Apply (op, args)) =
        let args' = List.map args ~f:(eval_all ctx) in
        let args_conc, args_abs =
          List.map args' ~f:(fun (Program.Apply ((_, c, a), _)) -> (c, a)) |> List.unzip
        in
        let conc = Value.eval ctx.ectx example op args_conc
        and abs = eval_restricted_single ctx example op args_abs in
        Apply ((op, conc, abs), args')
      in
      let p = eval_all ctx p in

      if debug then
        print_s
          [%message
            (p : (Op.t * Value.t * Set.M(Pred).t) Program.t) (ctx.preds : Set.M(Pred).t)];

      let (Apply ((_, conc, abs), _)) = p in
      let p' =
        strengthen_program ctx example p
          (strengthen_root ctx conc abs (contains_target example))
      in
      let preds =
        Program.iter p'
        |> Iter.map (fun ((_, _, _, ps), _) -> Iter.of_set ps)
        |> Iter.concat
        |> Iter.filter_map (function `True -> None | #Pred.t as p -> Some p)
        |> Iter.to_set (module Pred)
      in
      (if debug then
         let new_preds = Set.diff preds ctx.preds in
         Fmt.epr "@[<v>Added new predicates: %a\n%!@]" (Iter.pp_seq Pred.pp)
           (Iter.of_set new_preds));
      let preds' = Set.union ctx.preds preds in

      Some { ctx with preds = preds' }

    let is_error = List.exists ~f:(fun s -> Set.mem s `False)
  end

  exception Done of int * Lang.Op.t Program.t

  let synth ?initial_preds check_abs_example check_example ectx examples ops =
    let baseline_params = Baseline.Params.create ~verbose:debug ~max_cost:40 () in
    let rec loop iters ctx =
      let module Abs = struct
        include Lang

        module Value = struct
          include Abs_value

          (* When searching, only consider the predicates in the context *)
          let eval = eval_restricted ctx examples
        end

        let operators = ops
      end in
      let ctx' =
        let m_prog =
          Baseline.synthesize (module Abs) baseline_params
          @@ `Pred
               (fun op s ->
                 [%equal: Abs.Type.t] (Abs.Op.ret_type op) Abs.Type.output
                 && List.for_all2_exn examples s ~f:check_abs_example)
        in

        match m_prog with
        | Some p -> (
            Fmt.epr "Found program: %a\n%!" (Program.pp Lang.Op.pp) p;
            match
              List.find examples ~f:(fun example -> not (check_example p example))
            with
            | None -> raise @@ Done (iters, p)
            | Some example ->
                Fmt.epr "Refining...\n%!";
                Abs.Value.refine ctx example check_abs_example p)
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
    let ctx = Abs_value.Ctx.create ?initial_preds ectx in
    try
      ignore (loop 0 ctx : Abs_value.Ctx.t);
      None
    with Done (iters, p) ->
      eprint_s
        [%message
          "synthesis completed"
            (iters : int)
            (Program.size p : int)
            (p : Lang.Op.t Program.t)];
      Some p
end
