open Std

module Tensor_pred = struct
  open Tensor

  type t = N_dims of int | N_elems of int | Len of int | Elems of int list | Int of int
  [@@deriving compare, hash, sexp]

  let complete = Iter.singleton

  type arg = [ `True | `Concrete of Value.t | `Pred of t ]
  type ret = [ `False | `Concrete of Value.t | `Pred of t ]

  let n_elems n = `Pred (N_elems n)
  let n_elems_of t = n_elems @@ Tensor.n_elems t
  let n_dims n = `Pred (N_dims n)
  let n_dims_of t = n_dims @@ Tensor.n_dims t
  let elems xs = `Pred (Elems (List.dedup_and_sort ~compare:[%compare: int] xs))
  let len x = `Pred (Len x)
  let len_of x = len @@ List.length x
  let unwrap (`Pred x) = x

  let lift = function
    | Value.Tensor t -> Iter.of_list [ unwrap @@ n_dims_of t; unwrap @@ n_elems_of t ]
    | Vector v -> Iter.of_list [ unwrap @@ len_of v; unwrap @@ elems v ]
    | Error | Int _ -> Iter.empty

  let transfer_reshape ectx (x : arg) (x' : arg) : ret list =
    match (x, x') with
    | `Concrete (Tensor _ as t), `Concrete (Vector _ as v) -> (
        match Value.eval ectx Op.Reshape [ t; v ] with
        | Tensor t -> [ n_dims_of t; n_elems_of t; `Concrete (Tensor t) ]
        | Error -> [ `False ]
        | _ -> failwith "expected a tensor")
    | (`Pred (N_elems n) as p), `Concrete (Vector v) ->
        if List.length v > 0 && n = List.product v then [ p ] else [ `False ]
    | (`Pred (N_elems _) as p), _ -> [ p ]
    | _, `Concrete (Vector v) -> [ n_dims @@ List.length v; n_elems @@ List.product v ]
    | `Concrete (Tensor t), `Pred (Elems v) when List.product v > Tensor.n_elems t -> [ `False ]
    | `Concrete (Tensor t), _ -> [ n_elems_of t ]
    | _, `Pred (Len l) -> [ n_dims l ]
    | _ -> []

  let is_permutation v =
    let v = List.sort ~compare:[%compare: int] v in
    List.for_alli v ~f:(fun i x -> x = i + 1)

  let transfer_permute ectx (x : arg) (x' : arg) : ret list =
    match (x, x') with
    | `Concrete (Tensor t), `Pred (Len i) -> if Tensor.n_dims t = i then [ n_dims i; n_elems_of t ] else [ `False ]
    | `Concrete (Tensor _ as t), `Concrete (Vector _ as v) -> (
        match Value.eval ectx Op.Permute [ t; v ] with
        | Tensor _ as t' -> [ `Concrete t' ]
        | Error -> [ `False ]
        | _ -> failwith "expected a tensor")
    | `Pred (N_dims n), `Pred (Len i) -> if n = i then [ n_dims n ] else [ `False ]
    | `Concrete (Tensor t), `Pred (Elems v) when (not (is_permutation v)) || List.length v <> Tensor.n_dims t ->
        [ `False ]
    | `Pred (N_dims n), `Concrete (Vector v) -> if List.length v = n then [ n_dims n ] else [ `False ]
    | (`Pred (N_elems _ | N_dims _) as p), _ -> [ p ]
    | `Concrete (Tensor t), _ -> [ n_elems_of t; n_dims_of t ]
    | _, `Pred (Len l) -> [ n_dims l ]
    | _, `Pred (Elems v) when not (is_permutation v) -> [ `False ]
    | _, `Concrete (Vector v) -> if is_permutation v then [ n_dims @@ List.length v ] else [ `False ]
    | _ -> []

  let transfer_cons (x : arg) (v : arg) : ret list =
    match (x, v) with
    | `Concrete (Int x), `Concrete (Vector xs) -> [ `Concrete (Vector (x :: xs)); elems (x :: xs) ]
    | `Concrete (Int x), `Pred (Elems xs) -> [ elems (x :: xs) ]
    | _, `Pred (Len l) -> [ len (l + 1) ]
    | _, `Concrete (Vector v) -> [ len (List.length v + 1) ]
    | _ -> []

  let transfer_vec (x : arg) : ret list =
    match x with `Concrete (Int x) -> [ `Concrete (Vector [ x ]); elems [ x ]; len 1 ] | _ -> [ len 1 ]

  let transfer_int (x : int) : ret list = [ `Concrete (Int x) ]
  let transfer_id t : ret list = [ n_dims_of t; n_elems_of t; `Concrete (Tensor t) ]

  let transfer : _ -> _ -> arg list -> ret list =
   fun ectx op args ->
    match (op, args) with
    | Op.Id t, [] -> transfer_id t
    | Reshape, [ x; x' ] -> transfer_reshape ectx x x'
    | Permute, [ x; x' ] -> transfer_permute ectx x x'
    | Cons, [ x; x' ] -> transfer_cons x x'
    | Vec, [ x ] -> transfer_vec x
    | Int x, [] -> transfer_int x
    | Flip, _ -> assert false
    | _ -> failwith "unexpected args"

  let eval abs (conc : Value.t) =
    match (abs, conc) with
    | N_dims n, Tensor t -> Tensor.n_dims t = n
    | N_elems n, Tensor t -> Tensor.n_elems t = n
    | Len n, Vector v -> List.length v = n
    | Elems es, Vector v ->
        let es = Set.of_list (module Int) es in
        List.for_all v ~f:(Set.mem es)
    | _ -> failwith "unexpected predicate"

  let cost = function `Concrete _ -> 5 | `Pred (Elems _) -> 3 | `Pred _ -> 1
end

module Synth = Abstract_synth.Make (Tensor) (Tensor_pred)

let synth = Synth.synth (Tensor.Value.Ctx.create ())
