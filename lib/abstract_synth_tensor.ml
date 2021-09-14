module Abs_value = struct
  open Tensor

  module Pred = struct
    module T = struct
      type t =
        [ `False
        | `N_dims of int
        | `N_elems of int
        | `Concrete_t of Value.Tensor.t
        | `Len of int
        | `Concrete_v of Value.Vector.t
        | `Int of int ]
      [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)

    let cost = function `Concrete_t _ | `Concrete_v _ -> 2 | _ -> 1
  end

  module T = struct
    type t = Set.M(Pred).t [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  module Ctx = struct
    type t = { preds : Set.M(Pred).t } [@@deriving compare, sexp]

    let empty = { preds = Set.empty (module Pred) }

    let create () = empty

    let of_params _ = empty
  end

  let n_dims = Owl.Arr.num_dims

  let n_elems = Owl.Arr.numel

  let relevant = function
    | Value.Tensor t -> [ `N_dims (n_dims t); `N_elems (n_elems t); `Concrete_t t ]
    | Vector v -> [ `Len (List.length v); `Concrete_v v ]
    | Error -> [ `False ]
    | Int x -> [ `Int x ]

  let complete vs =
    Set.to_list vs
    |> List.concat_map ~f:(function
         | `Concrete_v v as p -> [ p; `Len (List.length v) ]
         | `Concrete_t t as p -> [ p; `N_dims (n_dims t); `N_elems (n_elems t) ]
         | p -> [ p ])
    |> Set.of_list (module Pred)

  let implies p p' = Set.is_subset (complete p) ~of_:(complete p')

  let lift (ctx : Ctx.t) v = Set.inter ctx.preds (Set.of_list (module Pred) @@ relevant v)

  let eval_pred = function `False -> false | _ -> true

  let eval_tensor_pred t = function
    | `Concrete_t t' -> Owl.Arr.equal t t'
    | `N_dims n -> n_dims t = n
    | `N_elems n -> n_elems t = n
    | p -> eval_pred p

  let eval_vector_pred v = function
    | `Concrete_v v' -> [%compare.equal: Value.Vector.t] v v'
    | `Len l -> List.length v = l
    | p -> eval_pred p

  let eval_int_pred x = function `Int x' -> x = x' | p -> eval_pred p

  let implies_not_value (p : Set.M(Pred).t) = function
    | Value.Tensor t -> not (Set.for_all p ~f:(eval_tensor_pred t))
    | Vector x -> not (Set.for_all p ~f:(eval_vector_pred x))
    | Int x -> not (Set.for_all p ~f:(eval_int_pred x))
    | Error -> not (Set.for_all p ~f:eval_pred)

  let contains abs conc = match conc with Value.Tensor t' -> Set.for_all abs ~f:(eval_tensor_pred t') | _ -> false

  let eval (ctx : Ctx.t) op args =
    match (op, args) with
    | Op.Id t, [] -> lift ctx (Tensor t)
    | Reshape, [ m; v ] ->
        let m_preds = Set.to_list m |> Iter.of_list and v_preds = Set.to_list v |> Iter.of_list in
        Iter.product m_preds v_preds
        |> Iter.filter_map (function
             | `False, _ | _, `False -> Some `False
             | `Concrete_t t', `Concrete_v v' -> (
                 match Value.eval () Op.Reshape [ Value.Tensor t'; Value.Vector v' ] with
                 | Tensor t'' -> Some (`Concrete_t t'')
                 | Error -> Some `False
                 | _ -> failwith "expected a tensor")
             | _, `Len i' -> Some (`N_dims i')
             | _, `Concrete_v v' -> Some (`N_dims (List.length v'))
             | (`N_elems _ as p), _ -> Some p
             | `Concrete_t t', _ -> Some (`N_elems (Owl.Arr.numel t'))
             | _ -> None)
        |> Iter.to_list
        |> Set.of_list (module Pred)
        |> Set.inter ctx.preds
    | Permute, [ m; v ] ->
        let m_preds = Set.to_list m |> Iter.of_list and v_preds = Set.to_list v |> Iter.of_list in
        Iter.product m_preds v_preds
        |> Iter.filter_map (function
             | `False, _ | _, `False -> Some `False
             | _, `Len i' -> Some (`N_dims i')
             | (`N_elems _ as p), _ -> Some p
             | `Concrete_t t', `Concrete_v v' -> (
                 match Value.eval () Op.Permute [ Value.Tensor t'; Value.Vector v' ] with
                 | Tensor t'' -> Some (`Concrete_t t'')
                 | Error -> Some `False
                 | _ -> failwith "expected a tensor")
             | _, `Concrete_v v' -> Some (`N_dims (List.length v'))
             | `Concrete_t t', _ -> Some (`N_elems (Owl.Arr.numel t'))
             | _ -> None)
        |> Iter.to_list
        |> Set.of_list (module Pred)
        |> Set.inter ctx.preds
    | Flip, [ m; _ ] -> m
    | Cons, [ x; ps ] ->
        let x_preds = Set.to_list x |> Iter.of_list and ps_preds = Set.to_list ps |> Iter.of_list in
        Iter.product x_preds ps_preds
        |> Iter.filter_map (function
             | `False, _ | _, `False -> Some `False
             | `Int x, `Concrete_v xs -> Some (`Concrete_v (x :: xs))
             | _, `Len l -> Some (`Len (l + 1))
             | _ -> None)
        |> Iter.to_list
        |> Set.of_list (module Pred)
        |> Set.inter ctx.preds
    | Vec, [ x ] ->
        Set.to_list x
        |> List.concat_map ~f:(function `Int x -> [ `Len 1; `Concrete_v [ x ] ] | _ -> [])
        |> Set.of_list (module Pred)
        |> Set.inter ctx.preds
    | Int x, [] -> Set.singleton (module Pred) (`Int x)
    | op, args -> raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)]

  let conjuncts ?(k = 3) l =
    let k = min k (List.length l) in
    Iter.(0 -- k)
    |> Iter.map (fun k -> if k = 0 then Iter.empty else Combinat.combinations ~k l)
    |> Iter.concat
    |> Iter.map (fun cs -> (Array.sum (module Int) cs ~f:Pred.cost, cs))
    |> Iter.sort ~cmp:(fun (c, _) (c', _) -> [%compare: int] c' c)

  let conjuncts = function
    | [] -> Iter.empty
    | [ x ] -> conjuncts x |> Iter.map (fun (c, x) -> (c, [ x ]))
    | [ x; x' ] ->
        Iter.product (conjuncts x) (conjuncts x')
        |> Iter.map (fun ((c, x), (c', x')) -> (c + c', [ x; x' ]))
        |> Iter.sort ~cmp:(fun (c, _) (c', _) -> [%compare: int] c' c)
    | _ -> assert false

  let strengthen too_strong too_weak implies =
    let relevant = List.map too_strong ~f:relevant in
    let candidates =
      conjuncts relevant
      |> Iter.map (fun (_, cs) -> List.map cs ~f:(Set.of_array (module Pred)))
      |> Iter.map (fun cs -> List.map2_exn cs too_weak ~f:Set.union)
      |> Iter.to_list
    in
    List.find_exn ~f:implies candidates

  let strengthen_root too_strong too_weak target =
    match
      strengthen [ too_strong ] [ too_weak ] (function [ p ] -> implies_not_value p target | _ -> assert false)
    with
    | [ p ] -> p
    | _ -> assert false

  let strengthen_children ctx op conc abs out = strengthen conc abs (fun cs -> implies (eval ctx op cs) out)

  let rec strengthen_program ctx (Program.Apply ((op, conc, abs), args)) out =
    let args' =
      if List.is_empty args then []
      else
        let out' =
          let args_conc, args_abs = List.map args ~f:(fun (Program.Apply ((_, c, a), _)) -> (c, a)) |> List.unzip in
          strengthen args_conc args_abs (fun cs -> implies (eval ctx op cs) out)
        in
        List.map2_exn args out' ~f:(strengthen_program ctx)
    in
    Program.Apply ((op, conc, abs, out), args')

  let rec eval_all ctx (Program.Apply (op, args)) =
    let args' = List.map args ~f:(eval_all ctx) in
    let args_conc, args_abs = List.map args' ~f:(fun (Program.Apply ((_, c, a), _)) -> (c, a)) |> List.unzip in
    let conc = Value.eval () op args_conc and abs = eval ctx op args_abs in
    Apply ((op, conc, abs), args')

  let refine ctx (target : Value.t) p : Ctx.t option =
    print_s [%message "refining" (p : Op.t Program.t)];
    let p = eval_all ctx p in
    let (Apply ((_, conc, abs), _)) = p in
    let p' = strengthen_program ctx p (strengthen_root conc abs target) in
    let preds =
      Program.ops p' |> List.concat_map ~f:(fun (_, _, _, ps) -> Set.to_list ps) |> Set.of_list (module Pred)
    in
    Some { preds = Set.union ctx.preds preds }

  let embed _ = failwith ""

  let dist _ = failwith ""
end

let synth target ops =
  let module Conc = Tensor in
  let module Abs_bench = Bench.Make (Conc.Op) (Abs_value) in
  let module Abs = struct
    include Conc
    module Value = Abs_value
    module Bench = Abs_bench

    let bench = Dumb_params.Spec.add spec Bench.param
  end in
  let exception Done in
  let module Synth = Baseline.Make (Abs) in
  let rec loop ctx =
    let ctx' =
      let abs_target = Abs.Value.lift ctx target in
      print_s [%message "abstract target" (abs_target : Abs_value.t)];
      let sctx = Synth.Ctx.create ~max_cost:15 ctx ops abs_target in
      let synth =
        object
          inherit Synth.synthesizer sctx

          method! check_states states =
            List.iter states ~f:(fun (s, op, args) ->
                if [%compare.equal: Abs.Type.t] (Abs.Op.ret_type op) Abs.Type.output && Abs.Value.contains s target then
                  let p = Synth.Search_state.program_of_op_args_exn search_state op args in
                  raise @@ Synth.Done p)
        end
      in
      match synth#run with
      | Some p ->
          let v = Program.eval (Conc.Value.eval ()) p in
          if [%compare.equal: Conc.Value.t] v target then raise Done else Abs.Value.refine ctx target p
      | None -> failwith "synthesis failed"
    in
    match ctx' with
    | Some ctx' ->
        if [%compare.equal: Abs.Value.Ctx.t] ctx ctx' then failwith "refinement failed";
        print_s [%message "refined" (ctx' : Abs.Value.Ctx.t)];
        loop ctx'
    | None -> failwith "refinement failed"
  in
  let ctx = Abs.Value.Ctx.create () in
  try ignore (loop ctx : Abs_value.Ctx.t) with Done -> ()
