open Std

module Abs_value = struct
  open Tensor

  module Pred = struct
    module T = struct
      type any_pred = [ `False ] [@@deriving compare, hash, sexp]

      type int_pred = [ any_pred | `Int of int ] [@@deriving compare, hash, sexp]

      type vector_pred = [ any_pred | `Len of int | `Elems of int list | `Concrete_v of Value.Vector.t ]
      [@@deriving compare, hash, sexp]

      type tensor_pred = [ any_pred | `N_dims of int | `N_elems of int | `Concrete_t of Value.Tensor.t ]
      [@@deriving compare, hash, sexp]

      type t = [ int_pred | vector_pred | tensor_pred ] [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)

    let cost = function `Concrete_t _ | `Concrete_v _ -> 5 | `Elems _ -> 3 | _ -> 1
  end

  module T = struct
    type t = Set.M(Pred).t [@@deriving compare, equal, hash, sexp]

    let of_list l =
      if List.mem ~equal:[%compare.equal: Pred.t] l `False then Set.singleton (module Pred) `False
      else Set.of_list (module Pred) l
  end

  include T
  include Comparator.Make (T)

  module Ctx = struct
    type nonrec t = {
      preds : Set.M(Pred).t;
      ectx : (Value.Ctx.t[@opaque]);
      refine_log : ((Op.t * Value.t * t) Program.t * Set.M(Pred).t * Set.M(Pred).t) Queue.t;
    }
    [@@deriving sexp]

    let create ?ectx () =
      {
        preds = Set.empty (module Pred);
        ectx = Option.value_lazy ~default:(lazy (Value.Ctx.create ())) ectx;
        refine_log = Queue.create ();
      }

    let of_params _ = failwith ""
  end

  let relevant = function
    | Value.Tensor t -> [ `N_dims (Tensor.n_dims t); `N_elems (Tensor.n_elems t); `Concrete_t t ]
    | Vector v -> [ `Len (List.length v); `Concrete_v v; `Elems (List.dedup_and_sort ~compare:[%compare: int] v) ]
    | Error -> [ `False ]
    | Int x -> [ `Int x ]

  let complete vs =
    Set.to_list vs
    |> List.concat_map ~f:(function
         | `Concrete_v v as p -> [ p; `Len (List.length v); `Elems (List.dedup_and_sort ~compare:[%compare: int] v) ]
         | `Concrete_t t as p -> [ p; `N_dims (Tensor.n_dims t); `N_elems (Tensor.n_elems t) ]
         | p -> [ p ])
    |> Set.of_list (module Pred)

  let implies p p' =
    let ret =
      if Set.mem p `False then true
      else if Set.mem p' `False then false
      else Set.is_subset (complete p') ~of_:(complete p)
    in
    (* print_s [%message "implies" (p : t) (p' : t) (ret : bool)]; *)
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
    | Op.Id t, [] -> of_list @@ relevant @@ Tensor t
    | Reshape, [ m; v ] ->
        let prod = List.reduce_exn ~f:( * ) in
        let m_preds =
          Iter.of_set m
          |> Iter.map (function
               | (`False | `N_elems _) as p -> [ p ]
               | `Concrete_t t -> [ `N_elems (Tensor.n_elems t) ]
               | `N_dims _ -> []
               | #Pred.int_pred | #Pred.vector_pred -> failwith "unexpected predicate")
        in
        let v_preds =
          Iter.of_set v
          |> Iter.map (function
               | `False as p -> [ p ]
               | `Len l -> [ `N_dims l ]
               | `Concrete_v v -> [ `N_dims (List.length v); `N_elems (List.product v) ]
               | `Elems _ -> []
               | #Pred.int_pred | #Pred.tensor_pred -> failwith "unexpected predicate")
        in
        let pair_preds =
          Iter.product (Iter.of_set m) (Iter.of_set v)
          |> Iter.map (function
               | `Concrete_t t', `Concrete_v v' -> (
                   match Value.eval ctx.ectx Op.Reshape [ Value.Tensor t'; Value.Vector v' ] with
                   | Tensor t'' -> [ `Concrete_t t''; `N_dims (Tensor.n_dims t''); `N_elems (Tensor.n_elems t'') ]
                   | Error -> [ `False ]
                   | _ -> failwith "expected a tensor")
               | `N_elems n, `Concrete_v v -> if List.length v > 0 && n = prod v then [ `N_elems n ] else [ `False ]
               | `Concrete_t t, `Elems v -> if prod v > Tensor.n_elems t then [ `False ] else []
               | _ -> [])
        in
        Iter.append_l [ m_preds; v_preds; pair_preds ] |> Iter.to_list |> List.concat |> of_list
    | Permute, [ m; v ] ->
        let is_permutation v =
          let v = List.sort ~compare:[%compare: int] v in
          List.for_alli v ~f:(fun i x -> x = i + 1)
        in

        let m_preds =
          Iter.of_set m
          |> Iter.map (function
               | (`False | `N_elems _ | `N_dims _) as p -> [ p ]
               | `Concrete_t t -> [ `N_elems (Tensor.n_elems t); `N_dims (Tensor.n_dims t) ]
               | #Pred.int_pred | #Pred.vector_pred -> failwith "unexpected predicate")
        in
        let v_preds =
          Iter.of_set v
          |> Iter.map (function
               | `False as p -> [ p ]
               | `Len l -> [ `N_dims l ]
               | `Elems v -> if is_permutation v then [] else [ `False ]
               | `Concrete_v v -> if is_permutation v then [ `N_dims (List.length v) ] else [ `False ]
               | #Pred.int_pred | #Pred.tensor_pred -> failwith "unexpected predicate")
        in

        let pair_preds =
          Iter.product (Iter.of_set m) (Iter.of_set v)
          |> Iter.map (function
               | `Concrete_t t, `Len i ->
                   if Tensor.n_dims t = i then [ `N_dims i; `N_elems (Tensor.n_elems t) ] else [ `False ]
               | `Concrete_t t, `Concrete_v v -> (
                   match Value.eval ctx.ectx Op.Permute [ Value.Tensor t; Value.Vector v ] with
                   | Tensor t' -> [ `Concrete_t t' ]
                   | Error -> [ `False ]
                   | _ -> failwith "expected a tensor")
               | `N_dims n, `Len i -> if n = i then [ `N_dims n ] else [ `False ]
               | `Concrete_t t, `Elems v ->
                   if is_permutation v && List.length v = Tensor.n_dims t then [] else [ `False ]
               | `N_dims n, `Elems v | `N_dims n, `Concrete_v v ->
                   if List.length v = n then [ `N_dims n ] else [ `False ]
               | _ -> [])
        in

        Iter.append_l [ m_preds; v_preds; pair_preds ] |> Iter.to_list |> List.concat |> of_list
    | Flip, [ m; x ] ->
        Iter.product (Iter.of_set m) (Iter.of_set x)
        |> Iter.map (function
             | `False, #Pred.int_pred | #Pred.tensor_pred, `False -> [ `False ]
             | `Concrete_t t, `Int i -> (
                 match Value.eval ctx.ectx Op.Flip [ Value.Tensor t; Value.Int i ] with
                 | Tensor t' -> [ `Concrete_t t' ]
                 | Error -> [ `False ]
                 | _ -> failwith "expected a tensor")
             | (#Pred.tensor_pred as p), #Pred.int_pred -> [ p ]
             | (#Pred.vector_pred | #Pred.int_pred), (#Pred.tensor_pred | #Pred.vector_pred)
             | #Pred.tensor_pred, (#Pred.tensor_pred | #Pred.vector_pred)
             | (#Pred.int_pred | #Pred.vector_pred), #Pred.int_pred ->
                 failwith "unexpected predicate")
        |> Iter.to_list |> List.concat |> of_list
    | Cons, [ x; ps ] ->
        let ps_preds =
          Iter.of_set ps
          |> Iter.map (function
               | `False as p -> [ p ]
               | `Len l -> [ `Len (l + 1) ]
               | `Concrete_v v -> [ `Len (List.length v + 1) ]
               | `Elems _ -> []
               | #Pred.int_pred | #Pred.tensor_pred -> failwith "unexpected predicate")
        in

        let pair_preds =
          Iter.product (Iter.of_set x) (Iter.of_set ps)
          |> Iter.map (function
               | `False, #Pred.vector_pred | #Pred.int_pred, `False -> [ `False ]
               | `Int x, `Concrete_v xs ->
                   [ `Concrete_v (x :: xs); `Elems (List.dedup_and_sort ~compare:[%compare: int] (x :: xs)) ]
               | `Int x, `Elems xs -> [ `Elems (List.dedup_and_sort ~compare:[%compare: int] (x :: xs)) ]
               | _ -> [])
        in
        Iter.append_l [ ps_preds; pair_preds ] |> Iter.to_list |> List.concat |> of_list
    | Vec, [ x ] ->
        [ `Len 1 ]
        @ (Set.to_list x
          |> List.concat_map ~f:(function `Int x -> [ `Len 1; `Concrete_v [ x ]; `Elems [ x ] ] | _ -> []))
        |> of_list
    | Int x, [] -> Set.singleton (module Pred) (`Int x)
    | op, args -> raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)]

  let conjuncts ?(k = 3) l =
    let k = min k (List.length l) in
    let ret =
      Iter.(0 -- k)
      |> Iter.map (fun k -> if k = 0 then Iter.empty else Iter.map Array.to_list @@ Combinat.combinations ~k l)
      |> Iter.concat
      |> Iter.map (fun cs -> (List.sum (module Int) cs ~f:Pred.cost, cs))
      (* |> Iter.persistent *)
    in
    (* print_s [%message "conjuncts" (l : Pred.t list) (ret : (int * Pred.t list) Iter.t)]; *)
    ret

  let conjuncts = function
    | [] -> Iter.empty
    | [ x ] -> conjuncts x |> Iter.map (fun (c, x) -> (c, [ x ]))
    | [ x; x' ] -> Iter.product (conjuncts x) (conjuncts x') |> Iter.map (fun ((c, x), (c', x')) -> (c + c', [ x; x' ]))
    | _ -> assert false

  let strengthen too_strong too_weak implies =
    let candidates =
      List.map too_strong ~f:relevant |> conjuncts
      |> Iter.sort ~cmp:(fun (c, _) (c', _) -> [%compare: int] c c')
      |> Iter.map (fun (cost, cs) -> (cost, List.map cs ~f:of_list))
      |> Iter.to_list
    in
    (* print_s [%message (too_weak : t list) (candidates : (int * t list) list)]; *)
    let candidates = List.map ~f:(fun (_, cs) -> List.map2_exn cs too_weak ~f:Set.union) candidates in
    List.find_exn candidates ~f:implies

  let strengthen_root too_strong too_weak target =
    (*     print_s [%message "strengthen root" (too_strong : Value.t) (too_weak : Set.M(Pred).t) (target : Value.t)]; *)
    let new_pred =
      match
        strengthen [ too_strong ] [ too_weak ] (function [ p ] -> implies_not_value p target | _ -> assert false)
      with
      | [ p ] -> p
      | _ -> assert false
    in
    (*     print_s [%message (new_pred : Set.M(Pred).t)];*)
    new_pred

  let rec strengthen_program ctx (Program.Apply ((op, conc, abs), args)) out =
    let args' =
      if List.is_empty args then []
      else
        let args_conc, args_abs = List.map args ~f:(fun (Program.Apply ((_, c, a), _)) -> (c, a)) |> List.unzip in
        let out' =
          strengthen args_conc args_abs (fun cs ->
              let v = eval ctx op cs in
              (* print_s [%message "check implies" (op : Op.t) (cs : t list) (v : t) (out : t)]; *)
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
    (* print_s [%message "old program" (p : (Op.t * Value.t * t) Program.t)]; *)
    let p' = strengthen_program ctx p (strengthen_root conc abs target) in
    (* print_s [%message "new program" (p' : (Op.t * Value.t * t * t) Program.t)]; *)
    let preds =
      Program.ops p' |> List.concat_map ~f:(fun (_, _, _, ps) -> Set.to_list ps) |> Set.of_list (module Pred)
    in
    Queue.enqueue ctx.refine_log (p, ctx.preds, preds);

    Some { ctx with preds = Set.union ctx.preds preds }

  let is_error s = Set.mem s `False
end

let synth cost target ops =
  let module Conc = Tensor in
  let module Abs = struct
    include Conc
    module Value = Abs_value
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
        if [%compare.equal: Abs_value.t] ctx.preds ctx'.preds then
          raise_s [%message "refinement failed" (ctx : Abs.Value.Ctx.t) (ctx' : Abs.Value.Ctx.t)];
        (* print_s [%message (Set.diff ctx'.preds ctx.preds : Set.M(Abs_value.Pred).t)]; *)
        loop (iters + 1) ctx'
    | None -> failwith "refinement failed"
  in
  let ctx = Abs.Value.Ctx.create () in
  (try ignore (loop 0 ctx : Abs_value.Ctx.t)
   with Done (iters, p) ->
     eprint_s [%message "synthesis completed" (iters : int) (Program.size p : int) (p : Abs.Op.t Program.t)]);
  ctx.refine_log
