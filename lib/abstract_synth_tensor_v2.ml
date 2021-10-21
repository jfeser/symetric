open Std

module Tensor_pred = struct
  open Tensor

  type op = Op.t

  type ctx = Value.Ctx.t

  type concrete = Value.t

  type any_pred = [ `True ] [@@deriving compare, hash, sexp]

  type int_pred_conc = [ `Int of int ] [@@deriving compare, hash, sexp]

  type vector_pred = [ `Len of int | `Elems of int list ] [@@deriving compare, hash, sexp]

  type vector_pred_conc = [ vector_pred | `Concrete_v of Value.Vector.t ] [@@deriving compare, hash, sexp]

  type tensor_pred = [ `N_dims of int | `N_elems of int ] [@@deriving compare, hash, sexp]

  type tensor_pred_conc = [ tensor_pred | `Concrete_t of Value.Tensor.t ] [@@deriving compare, hash, sexp]

  type lifted = [ tensor_pred_conc | vector_pred_conc | int_pred_conc | `True ] [@@deriving compare, hash, sexp]

  type t = [ vector_pred | tensor_pred ] [@@deriving compare, hash, sexp]

  let relevant = function
    | Value.Tensor t -> [ `N_dims (Tensor.n_dims t); `N_elems (Tensor.n_elems t) ]
    | Vector v -> [ `Len (List.length v); `Elems (List.dedup_and_sort ~compare:[%compare: int] v) ]
    | Error -> []
    | Int _ -> []

  let complete = Iter.singleton

  let lift = function
    | `True -> `True
    | `Pred (#t as p) -> p
    | `Concrete x -> (
        match x with
        | Value.Tensor t -> `Concrete_t t
        | Vector v -> `Concrete_v v
        | Int x -> `Int x
        | Error -> failwith "unexpected error")

  let lower : _ -> [ `False | `Pred of t | `Concrete of _ ] = function
    | `False -> `False
    | `Concrete_t t -> `Concrete (Value.Tensor t)
    | `Concrete_v v -> `Concrete (Value.Vector v)
    | `Int x -> `Concrete (Value.Int x)
    | #t as p -> `Pred p

  let fail_args op args = raise_s [%message "unexpected arguments" (op : string) (args : _ list)]

  let tensor_vec_args op args f =
    match args with
    | [ (#tensor_pred_conc as t); (#vector_pred_conc as v) ] -> f t v
    | _ -> raise_s [%message "unexpected arguments" (op : Op.t) (args : lifted list)]

  let int_vec_args op args f =
    match args with
    | [ (#int_pred_conc as x); (#vector_pred_conc as v) ] -> f x v
    | _ -> raise_s [%message "unexpected arguments" (op : Op.t) (args : lifted list)]

  let int_args op args f =
    match args with
    | [ (#int_pred_conc as x) ] -> f x
    | _ -> raise_s [%message "unexpected arguments" (op : Op.t) (args : lifted list)]

  let no_args op args f =
    match args with [] -> f () | _ -> raise_s [%message "unexpected arguments" (op : Op.t) (args : lifted list)]

  let transfer_reshape ectx x x' =
    match (x, x') with
    | `Concrete_t t, `Concrete_v v -> (
        match Value.eval ectx Op.Reshape [ Tensor t; Vector v ] with
        | Tensor t -> [ `N_dims (Tensor.n_dims t); `N_elems (Tensor.n_elems t); `Concrete_t t ]
        | Error -> [ `False ]
        | _ -> failwith "expected a tensor")
    | (`N_elems n as p), `Concrete_v v -> if List.length v > 0 && n = List.product v then [ p ] else [ `False ]
    | (`N_elems _ as p), _ -> [ p ]
    | _, `Concrete_v v -> [ `N_dims (List.length v); `N_elems (List.product v) ]
    | `Concrete_t t, `Elems v -> if List.product v > Tensor.n_elems t then [ `False ] else []
    | `Concrete_t t, _ -> [ `N_elems (Tensor.n_elems t) ]
    | _, `Len l -> [ `N_dims l ]
    | _ -> []

  let is_permutation v =
    let v = List.sort ~compare:[%compare: int] v in
    List.for_alli v ~f:(fun i x -> x = i + 1)

  let transfer_permute ectx x x' =
    match (x, x') with
    | `Concrete_t t, `Len i -> if Tensor.n_dims t = i then [ `N_dims i; `N_elems (Tensor.n_elems t) ] else [ `False ]
    | `Concrete_t t, `Concrete_v v -> (
        match Value.eval ectx Op.Permute [ Value.Tensor t; Value.Vector v ] with
        | Tensor t' -> [ `Concrete_t t' ]
        | Error -> [ `False ]
        | _ -> failwith "expected a tensor")
    | `N_dims n, `Len i -> if n = i then [ `N_dims n ] else [ `False ]
    | `Concrete_t t, `Elems v -> if is_permutation v && List.length v = Tensor.n_dims t then [] else [ `False ]
    | `N_dims n, `Concrete_v v -> if List.length v = n then [ `N_dims n ] else [ `False ]
    | ((`N_elems _ | `N_dims _) as p), _ -> [ p ]
    | `Concrete_t t, _ -> [ `N_elems (Tensor.n_elems t); `N_dims (Tensor.n_dims t) ]
    | _, `Len l -> [ `N_dims l ]
    | _, `Elems v -> if is_permutation v then [] else [ `False ]
    | _, `Concrete_v v -> if is_permutation v then [ `N_dims (List.length v) ] else [ `False ]
    | _ -> []

  let transfer_cons x v =
    match (x, v) with
    | `Int x, `Concrete_v xs ->
        [ `Concrete_v (x :: xs); `Elems (List.dedup_and_sort ~compare:[%compare: int] (x :: xs)) ]
    | `Int x, `Elems xs -> [ `Elems (List.dedup_and_sort ~compare:[%compare: int] (x :: xs)) ]
    | _, `Len l -> [ `Len (l + 1) ]
    | _, `Concrete_v v -> [ `Len (List.length v + 1) ]
    | _ -> []

  let transfer_vec x = match x with `Int x -> [ `Concrete_v [ x ]; `Elems [ x ]; `Len 1 ] | _ -> [ `Len 1 ]

  let transfer_int x = [ `Int x ]

  let transfer_id t = [ `N_dims (Tensor.n_dims t); `N_elems (Tensor.n_elems t); `Concrete_t t ]

  let transfer :
      _ ->
      _ ->
      [ `Concrete of concrete | `Pred of t | `True ] list ->
      [ `Concrete of concrete | `False | `Pred of t ] list =
   fun ectx op args ->
    let ret =
      match (op, List.map ~f:lift args) with
      | Op.Id t, args -> no_args op args (fun () -> transfer_id t)
      | Reshape, args -> tensor_vec_args op args (transfer_reshape ectx)
      | Permute, args -> tensor_vec_args op args (transfer_permute ectx)
      | Cons, args -> int_vec_args op args transfer_cons
      | Vec, args -> int_args op args transfer_vec
      | Int x, args -> no_args op args (fun () -> transfer_int x)
      | Flip, _ -> assert false
    in
    List.map ~f:lower ret

  let eval_tensor_pred t = function
    | #tensor_pred as p -> (
        match p with
        | `Concrete_t t' -> [%compare.equal: Tensor.t] t t'
        | `N_dims n -> Tensor.n_dims t = n
        | `N_elems n -> Tensor.n_elems t = n)
    | p -> raise_s [%message "unexpected predicate" (p : t)]

  let eval_vector_pred v = function
    | #vector_pred as p -> (
        match p with
        | `Concrete_v v' -> [%compare.equal: Value.Vector.t] v v'
        | `Len l -> List.length v = l
        | `Elems es ->
            let es = Set.of_list (module Int) es in
            List.for_all v ~f:(Set.mem es))
    | p -> raise_s [%message "unexpected predicate" (p : t)]

  let eval abs conc =
    match conc with
    | Value.Tensor t -> eval_tensor_pred t abs
    | Value.Vector v -> eval_vector_pred v abs
    | Value.Int _ | Value.Error -> false

  let cost = function `Concrete _ -> 5 | `Pred (`Elems _) -> 3 | `Pred _ -> 1

  let lift c = relevant c |> Iter.of_list
end

module Synth = Abstract_synth.Make (Tensor) (Tensor_pred)

let synth = Synth.synth (Tensor.Value.Ctx.create ())
