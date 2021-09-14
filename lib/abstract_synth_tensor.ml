module Abs_value = struct
  open Tensor

  module Pred = struct
    module T = struct
      type any_pred = [ `False ] [@@deriving compare, hash, sexp]

      type int_pred = [ any_pred | `Int of int ] [@@deriving compare, hash, sexp]

      type vector_pred = [ any_pred | `Len of int | `Concrete_v of Value.Vector.t ] [@@deriving compare, hash, sexp]

      type tensor_pred = [ any_pred | `N_dims of int | `N_elems of int | `Concrete_t of Value.Tensor.t ]
      [@@deriving compare, hash, sexp]

      type t = [ int_pred | vector_pred | tensor_pred ] [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)

    let cost = function `Concrete_t _ | `Concrete_v _ -> 5 | _ -> 1
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

  let relevant = function
    | Value.Tensor t -> [ `N_dims (Tensor.n_dims t); `N_elems (Tensor.n_elems t); `Concrete_t t ]
    | Vector v -> [ `Len (List.length v); `Concrete_v v ]
    | Error -> [ `False ]
    | Int x -> [ `Int x ]

  let complete vs =
    Set.to_list vs
    |> List.concat_map ~f:(function
         | `Concrete_v v as p -> [ p; `Len (List.length v) ]
         | `Concrete_t t as p -> [ p; `N_dims (Tensor.n_dims t); `N_elems (Tensor.n_elems t) ]
         | p -> [ p ])
    |> Set.of_list (module Pred)

  let implies p p' =
    let ret = Set.is_subset (complete p) ~of_:(complete p') in
    ret

  let lift (ctx : Ctx.t) v = Set.inter ctx.preds (Set.of_list (module Pred) @@ relevant v)

  let eval_pred = function `False -> false | _ -> true

  let eval_tensor_pred t = function
    | `Concrete_t t' -> [%compare.equal: Tensor.t] t t'
    | `N_dims n -> Tensor.n_dims t = n
    | `N_elems n -> Tensor.n_elems t = n
    | p -> eval_pred p

  let eval_vector_pred v = function
    | `Concrete_v v' -> [%compare.equal: Value.Vector.t] v v'
    | `Len l -> List.length v = l
    | p -> eval_pred p

  let eval_int_pred x = function `Int x' -> x = x' | p -> eval_pred p

  let eval_error_pred _ = false

  let implies_not_value (p : Set.M(Pred).t) = function
    | Value.Tensor t -> not (Set.for_all p ~f:(eval_tensor_pred t))
    | Vector x -> not (Set.for_all p ~f:(eval_vector_pred x))
    | Int x -> not (Set.for_all p ~f:(eval_int_pred x))
    | Error -> not (Set.for_all p ~f:eval_error_pred)

  let contains abs conc = match conc with Value.Tensor t' -> Set.for_all abs ~f:(eval_tensor_pred t') | _ -> false

  let eval (ctx : Ctx.t) op args =
    match (op, args) with
    | Op.Id t, [] -> lift ctx (Tensor t)
    | Reshape, [ m; v ] ->
        let m_preds : Pred.t Iter.t = Set.to_list m |> Iter.of_list
        and v_preds : Pred.t Iter.t = Set.to_list v |> Iter.of_list in
        Iter.product m_preds v_preds
        |> Iter.map (function
             | `False, #Pred.vector_pred | #Pred.tensor_pred, `False -> [ `False ]
             | `Concrete_t t', `Concrete_v v' -> (
                 match Value.eval () Op.Reshape [ Value.Tensor t'; Value.Vector v' ] with
                 | Tensor t'' -> [ `Concrete_t t''; `N_dims (Tensor.n_dims t''); `N_elems (Tensor.n_elems t'') ]
                 | Error -> [ `False ]
                 | _ -> failwith "expected a tensor")
             | (`N_elems _ as p), #Pred.vector_pred -> [ p ]
             | `Concrete_t t, #Pred.vector_pred -> [ `N_elems (Tensor.n_elems t) ]
             | #Pred.tensor_pred, `Len i' -> [ `N_dims i' ]
             | #Pred.tensor_pred, `Concrete_v v' -> [ `N_dims (List.length v') ]
             | (#Pred.vector_pred | #Pred.int_pred), (#Pred.tensor_pred | #Pred.int_pred)
             | #Pred.tensor_pred, (#Pred.int_pred | #Pred.tensor_pred)
             | (#Pred.vector_pred | #Pred.int_pred), #Pred.vector_pred ->
                 failwith "Unexpected predicate")
        |> Iter.to_list |> List.concat
        |> Set.of_list (module Pred)
        |> Set.inter ctx.preds
    | Permute, [ m; v ] ->
        let m_preds = Set.to_list m |> Iter.of_list and v_preds = Set.to_list v |> Iter.of_list in
        Iter.product m_preds v_preds
        |> Iter.map (function
             | `False, #Pred.vector_pred | #Pred.tensor_pred, `False -> [ `False ]
             | `Concrete_t t, `Len i ->
                 if Tensor.n_dims t = i then [ `N_dims i; `N_elems (Tensor.n_elems t) ] else [ `False ]
             | `Concrete_t t, `Concrete_v v -> (
                 match Value.eval () Op.Permute [ Value.Tensor t; Value.Vector v ] with
                 | Tensor t' -> [ `Concrete_t t' ]
                 | Error -> [ `False ]
                 | _ -> failwith "expected a tensor")
             | `N_dims n, `Len i -> if n = i then [ `N_dims n ] else [ `False ]
             | _ -> [])
        |> Iter.to_list |> List.concat
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
    List.map too_strong ~f:relevant |> conjuncts
    |> Iter.map (fun (_, cs) -> List.map cs ~f:(Set.of_array (module Pred)))
    |> Iter.map (fun cs -> List.map2_exn cs too_weak ~f:Set.union)
    |> Iter.to_list |> List.find_exn ~f:implies

  let strengthen_root too_strong too_weak target =
    match
      strengthen [ too_strong ] [ too_weak ] (function [ p ] -> implies_not_value p target | _ -> assert false)
    with
    | [ p ] -> p
    | _ -> assert false

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

  let rec eval_all ctx (Program.Apply (op, args)) =
    let args' = List.map args ~f:(eval_all ctx) in
    let args_conc, args_abs = List.map args' ~f:(fun (Program.Apply ((_, c, a), _)) -> (c, a)) |> List.unzip in
    let conc = Value.eval () op args_conc and abs = eval ctx op args_abs in
    Apply ((op, conc, abs), args')

  let refine ctx (target : Value.t) p : Ctx.t option =
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
  let exception Done of Abs.Op.t Program.t in
  let module Synth = Baseline.Make (Abs) in
  let rec loop ctx =
    let ctx' =
      let abs_target = Abs.Value.lift ctx target in
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
          if [%compare.equal: Conc.Value.t] v target then (
            Synth.Search_state.print_stats synth#get_search_state;
            raise @@ Done p)
          else Abs.Value.refine ctx target p
      | None -> failwith "synthesis failed"
    in
    match ctx' with
    | Some ctx' ->
        if [%compare.equal: Abs.Value.Ctx.t] ctx ctx' then failwith "refinement failed"
        else print_s [%message "new preds" (Set.diff ctx'.preds ctx.preds : Abs.Value.t)];
        loop ctx'
    | None -> failwith "refinement failed"
  in
  let ctx = Abs.Value.Ctx.create () in
  try ignore (loop ctx : Abs_value.Ctx.t)
  with Done p -> print_s [%message "synthesis completed" (p : Abs.Op.t Program.t)]
