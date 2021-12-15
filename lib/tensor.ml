let name = "tensor"

module Type = struct
  module T = struct
    type t = Tensor | Vector | Int [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let output = Tensor
end

module Tensor = struct
  type t = { elems : int list; shape : int list } [@@deriving compare, equal, hash, sexp]

  let shape t = t.shape
  let n_elems t = List.length t.elems
  let n_dims t = List.length t.shape

  module T = Owl.Dense.Ndarray.D

  let to_owl t = T.of_array (Array.of_list @@ List.map ~f:Float.of_int t.elems) (Array.of_list t.shape)

  let of_owl arr =
    { elems = T.to_array arr |> Array.to_list |> List.map ~f:Float.to_int; shape = T.shape arr |> Array.to_list }

  let reshape t shape =
    try
      if List.fold ~f:( * ) ~init:1 shape = n_elems t && List.length shape <= 16 then
        Some (of_owl @@ T.reshape (to_owl t) (Array.of_list shape))
      else None
    with _ -> raise_s [%message (shape : int list)]

  let permute t dims =
    if [%compare.equal: int list] (List.sort ~compare:[%compare: int] dims) (List.init (n_dims t) ~f:(fun i -> i + 1))
    then
      let axis = List.map dims ~f:(fun d -> d - 1) |> Array.of_list in
      try Some (of_owl @@ T.transpose ~axis @@ to_owl t)
      with Failure msg -> raise_s [%message (msg : string) (t.shape : int list) (axis : int array)]
    else None

  let flip t axis = if axis >= 0 && axis < n_dims t then Some (of_owl @@ T.flip ~axis @@ to_owl t) else None
end

module Op = struct
  type type_ = Type.t

  module T = struct
    type t = Id of Tensor.t | Reshape | Permute | Flip | Cons | Vec | Int of int
    [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let ret_type = function Id _ | Reshape | Permute | Flip -> Type.Tensor | Cons | Vec -> Vector | Int _ -> Int

  let args_type = function
    | Id _ | Int _ -> []
    | Reshape | Permute -> [ Type.Tensor; Vector ]
    | Flip -> [ Tensor; Int ]
    | Cons -> [ Int; Vector ]
    | Vec -> [ Int ]

  let type_ x = (args_type x, ret_type x)
  let arity x = List.length @@ args_type x

  let rec vec_of_list = function
    | [] -> failwith "not enough elements"
    | [ x ] -> Program.(apply Vec ~args:[ apply (Int x) ])
    | x :: xs -> Program.(apply Cons ~args:[ apply (Int x); vec_of_list xs ])

  let cost _ = 1
  let is_commutative _ = false
  let pp = Fmt.nop
end

module Value = struct
  module Tensor = Tensor

  module Vector = struct
    type t = int list [@@deriving compare, hash, equal, sexp]
  end

  module T = struct
    type t = Tensor of Tensor.t | Vector of Vector.t | Int of int | Error [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let pp = Fmt.nop

  module Op_args = struct
    type nonrec t = Op.t * t list [@@deriving compare, hash, sexp]
  end

  module Ctx = struct
    type nonrec t = { memo : t Hashtbl.M(Op_args).t }

    let create () = { memo = Hashtbl.create (module Op_args) }
    let of_params _ = create ()
  end

  let eval _ op args =
    let of_tensor_opt m = Option.value ~default:Error @@ Option.map ~f:(fun t -> Tensor t) m in
    match (op, args) with
    | Op.Id t, [] -> Tensor t
    | Reshape, [ Tensor m; Vector v ] -> of_tensor_opt @@ Tensor.reshape m v
    | Permute, [ Tensor m; Vector v ] -> of_tensor_opt @@ Tensor.permute m v
    | Flip, [ Tensor t; Int a ] -> of_tensor_opt @@ Tensor.flip t a
    | Cons, [ Int x; Vector xs ] -> Vector (x :: xs)
    | Vec, [ Int x ] -> Vector [ x ]
    | Int x, [] -> Int x
    | (Reshape | Permute | Flip), ([ Error; _ ] | [ _; Error ]) -> Error
    | op, args -> raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)]

  let is_error = function Error -> true | _ -> false
end

module Bench0 = struct
  type t = (Op.t, Value.t) Bench.t [@@deriving compare, sexp]

  let solution_exn = Bench.solution_exn
  let output = Bench.output
  let ops = Bench.ops
  let create = Bench.create
  let load = Bench.load [%of_sexp: Op.t] [%of_sexp: Value.t]
  let save = Bench.save [%sexp_of: Op.t] [%sexp_of: Value.t]
end

include struct
  open Dumb_params

  let spec = Spec.create ()
  let _ = (Spec.add spec @@ Param.const_str ~name:"lang" name : (string, Param.bound) Param.t)
  let bench = Spec.add spec @@ Bench.param (module Op) (module Value)
end

module Bench = Bench0
