open Std

module type Lang_intf = sig
  module Type : sig
    type t [@@deriving compare, hash, sexp]

    include Comparator.S with type t := t

    val output : t
  end

  module Op : Op_intf.S with type type_ = Type.t

  module Value : sig
    type t [@@deriving compare, equal, hash, sexp]

    module Ctx : sig
      type t

      val of_params : Params.t -> t
    end

    include Comparator.S with type t := t

    val eval : Ctx.t -> Op.t -> t list -> t

    val is_error : t -> bool
  end

  module Bench : sig
    type t [@@deriving of_sexp]

    val ops : t -> Op.t list

    val output : t -> Value.t
  end

  val bench : (Bench.t, Dumb_params.Param.bound) Dumb_params.Param.t
end

module Base_pred = struct
  type ('v, 'p) t = True | False | Concrete of 'v | Pred of 'p [@@deriving compare, hash, sexp]
end

module type Domain_pred_intf = sig
  type concrete

  type op

  type t [@@deriving compare, hash, sexp]

  val cost : (concrete, t) Base_pred.t -> int

  val relevant : concrete -> t Iter.t

  val complete : t -> t Iter.t

  val eval : t -> concrete -> bool

  val transfer : op -> (concrete, t) Base_pred.t list -> (concrete, t) Base_pred.t
end

module Make
    (Lang : Lang_intf)
    (Domain_pred : Domain_pred_intf with type concrete = Lang.Value.t and type op = Lang.Op.t) =
struct
  open Lang
  open Base_pred

  module Pred = struct
    module T = struct
      type t = (Value.t, Domain_pred.t) Base_pred.t [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)

    let cost = Domain_pred.cost
  end

  module Ctx = struct
    type t = { preds : Set.M(Pred).t; max_conjuncts : int; ectx : (Value.Ctx.t[@opaque]) [@compare.ignore] }
    [@@deriving compare, sexp]

    let create ?(max_conjuncts = 3) ectx () = { preds = Set.empty (module Pred); max_conjuncts; ectx }
  end

  module T = struct
    type t = Bottom | Preds of Set.M(Pred).t [@@deriving compare, equal, hash, sexp]

    let of_iter ?(ctx : Ctx.t option) iter =
      let preds = Iter.fold Set.add (Set.empty (module Pred)) iter in
      if Set.is_empty preds || Set.mem preds False then Bottom
      else
        let preds = match ctx with Some ctx -> Set.inter preds ctx.preds | None -> preds in
        Preds (Set.remove preds True)

    let to_iter = function Bottom -> Iter.empty | Preds ps -> Iter.cons True @@ Iter.of_set ps

    let relevant conc =
      let default = Iter.of_list [ True; Concrete conc ] in
      let domain = Domain_pred.relevant conc |> Iter.map (fun p -> Pred p) in
      Iter.append default domain

    let complete p =
      Iter.of_set p
      |> Iter.map (function
           | (True | False) as p -> Iter.singleton p
           | Concrete v as p -> Iter.cons p @@ relevant v
           | Pred p -> Domain_pred.complete p |> Iter.map (fun p -> Pred p))
      |> Iter.concat |> Iter.to_list
      |> Set.of_list (module Pred)

    let implies p p' =
      match (p, p') with
      | Bottom, _ -> true
      | _, Bottom -> false
      | Preds ps, Preds ps' -> Set.is_subset (complete ps') ~of_:(complete ps)

    let lift ?ctx v = relevant v |> of_iter ?ctx

    let length = function Bottom -> 0 | Preds ps -> Set.length ps

    let and_ p p' =
      match (p, p') with Bottom, _ | _, Bottom -> Bottom | Preds ps, Preds ps' -> Preds (Set.union ps ps')

    let singleton p = of_iter @@ Iter.singleton p
  end

  include T
  include Comparator.Make (T)

  let eval ctx op args =
    List.map args ~f:to_iter |> Iter.list_product |> Iter.map (Domain_pred.transfer op) |> of_iter ~ctx

  let conjuncts (ctx : Ctx.t) ~n_args too_strong =
    let module Conjunct_lang = struct
      module Type = struct
        type t = Args | Pred of int [@@deriving compare, hash, sexp]

        let output = Args
      end

      module Op = struct
        type t = Args | And of int | Pred of int * Pred.t [@@deriving compare, hash, sexp]

        let arity = function Args -> n_args | And _ -> 2 | Pred _ -> 0

        let ret_type : _ -> Type.t = function Args -> Args | And slot -> Pred slot | Pred (slot, _) -> Pred slot

        let args_type = function
          | Args -> List.init n_args ~f:(fun slot -> Type.Pred slot)
          | And slot -> [ Type.Pred slot; Type.Pred slot ]
          | Pred _ -> []

        let cost = function Pred (_, p) -> Pred.cost p | Args | And _ -> 0
      end

      module Value = struct
        type nonrec t = Args of t list | Pred of t [@@deriving compare, hash, sexp]

        module Ctx = struct
          type t = unit
        end

        let eval _ op args =
          match (op, args) with
          | Op.Args, preds -> Args (List.map preds ~f:(function Pred p -> p | _ -> assert false))
          | And _, [ Pred p; Pred p' ] -> Pred (and_ p p')
          | Pred (_, p), [] -> Pred (singleton p)
          | _ -> assert false

        let is_error = function Pred p -> length p > ctx.max_conjuncts | _ -> false
      end
    end in
    let open Conjunct_lang in
    let module Synth = Baseline.Make (Conjunct_lang) in
    let ops =
      Op.Args
      :: List.concat_mapi too_strong ~f:(fun slot conc ->
             let pred_ops = relevant conc |> Iter.map (fun p -> Op.Pred (slot, p)) |> Iter.to_list in
             Op.And slot :: pred_ops)
    in
    let exception Done of t list in
    let synth =
      let config =
        Synth.Ctx.create () ops
          (`Pred
            (fun _ v -> match v with Value.Args args -> if f args then raise (Done args) else false | _ -> false))
      in
      new Synth.synthesizer config
    in
    try
      let (_ : _) = synth#run in
      raise_s [%message "failed to refine"]
    with Done args -> args

  (* let strengthen ctx too_strong too_weak implies = *)
  (*   let candidates = *)
  (*     List.map too_strong ~f:relevant |> Iter.list_product |> Iter.map (fun conj -> *)
  (*         let costs, preds = List.unzip conj in *)
  (*                 ) *)
  (*     |> Iter.sort ~cmp:(fun (c, _) (c', _) -> [%compare: int] c c') *)
  (*     |> Iter.map (fun (cost, cs) -> (cost, List.map cs ~f:of_list)) *)
  (*     |> Iter.to_list *)
  (*   in *)
  (*   let candidates = List.map ~f:(fun (_, cs) -> List.map2_exn cs too_weak ~f:Set.union) candidates in *)
  (*   List.find_exn candidates ~f:implies *)

  let strengthen_root too_strong too_weak target =
    let new_pred =
      match
        strengthen [ too_strong ] [ too_weak ] (function [ p ] -> implies_not_value p target | _ -> assert false)
      with
      | [ p ] -> p
      | _ -> assert false
    in
    new_pred

  let rec strengthen_program ctx (Program.Apply ((op, conc, abs), args)) out =
    let args' =
      if List.is_empty args then []
      else
        let args_conc, args_abs = List.map args ~f:(fun (Program.Apply ((_, c, a), _)) -> (c, a)) |> List.unzip in
        let out' =
          strengthen args_conc args_abs (fun cs ->
              let v = eval ctx op cs in
              implies v out)
        in
        List.map2_exn args out' ~f:(strengthen_program ctx)
    in
    Program.Apply ((op, conc, abs, out), args')

  let rec eval_all (ctx : Ctx.t) (Program.Apply (op, args)) =
    let args' = List.map args ~f:(eval_all ctx) in
    let args_conc, args_abs = List.map args' ~f:(fun (Program.Apply ((_, c, a), _)) -> (c, a)) |> List.unzip in
    let conc = Value.eval ctx.ectx op args_conc and abs = eval ctx op args_abs |> Set.inter ctx.preds in
    Apply ((op, conc, abs), args')

  let refine (ctx : Ctx.t) (target : Value.t) p : Ctx.t option =
    let p = eval_all ctx p in
    let (Apply ((_, conc, abs), _)) = p in
    let p' = strengthen_program ctx p (strengthen_root conc abs target) in
    let preds =
      Program.ops p' |> List.concat_map ~f:(fun (_, _, _, ps) -> Set.to_list ps) |> Set.of_list (module Pred)
    in
    Some { ctx with preds = Set.union ctx.preds preds }

  let is_error s = Set.mem s `False
end

let synth cost target ops =
  let module Conc = Tensor in
  let module Abs_bench = Bench.Make (Conc.Op) (Abs_value) in
  let module Abs = struct
    include Conc
    module Value = Abs_value
    module Bench = Abs_bench

    let bench = Dumb_params.Spec.add spec Bench.param
  end in
  let exception Done of int * Abs.Op.t Program.t in
  let module Synth = Baseline.Make (Abs) in
  let rec loop iters ctx =
    let ctx' =
      let sctx =
        Synth.Ctx.create ~max_cost:cost ctx ops
        @@ `Pred
             (fun op s ->
               [%compare.equal: Abs.Type.t] (Abs.Op.ret_type op) Abs.Type.output && Abs.Value.contains s target)
      in
      let synth =
        object
          inherit Synth.synthesizer sctx as super

          method! generate_states cost =
            super#generate_states cost |> List.map ~f:(fun (value, op, args) -> (Set.inter value ctx.preds, op, args))
        end
      in

      match synth#run with
      | Some p ->
          let v = Program.eval (Conc.Value.eval ctx.ectx) p in
          if [%compare.equal: Conc.Value.t] v target then (
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
        (* print_s [%message (Set.diff ctx'.preds ctx.preds : Set.M(Abs_value.Pred).t)]; *)
        loop (iters + 1) ctx'
    | None -> failwith "refinement failed"
  in
  let ctx = Abs.Value.Ctx.create () in
  try ignore (loop 0 ctx : Abs_value.Ctx.t)
  with Done (iters, p) ->
    eprint_s [%message "synthesis completed" (iters : int) (Program.size p : int) (p : Abs.Op.t Program.t)]
