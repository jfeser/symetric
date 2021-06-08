let name = "tensor"

module Type = struct
  module T = struct
    type t = Tensor | Vector | Int [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)
end

module Tensor = struct
  open Owl
  module M = Dense.Ndarray.Generic

  type t = Arr.arr

  let equal = M.equal

  let compare a a' = if equal a a' then 0 else if M.greater a a' then 1 else -1

  let sexp_of_t a = [%sexp_of: float array * int array] (M.to_array a, M.shape a)

  let t_of_sexp s =
    let data, shape = [%of_sexp: float array * int array] s in
    Arr.of_array data shape

  let hash_fold_t s t =
    let s = ref s in
    Arr.iter (fun x -> s := [%hash_fold: float] !s x) t;
    !s
end

module Op = struct
  type type_ = Type.t

  module T = struct
    type t =
      | Id of Tensor.t
      | Reshape
      | Permute
      | Flip
      | Cons
      | Vec
      | Int of int
    [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let ret_type = function
    | Id _ | Reshape | Permute | Flip -> Type.Tensor
    | Cons | Vec -> Vector
    | Int _ -> Int

  let args_type = function
    | Id _ | Int _ -> []
    | Reshape | Permute -> [ Type.Tensor; Vector ]
    | Flip -> [ Tensor; Int ]
    | Cons -> [ Int; Vector ]
    | Vec -> [ Int; Int ]

  let type_ x = (args_type x, ret_type x)

  let arity x = List.length @@ args_type x
end

module Value = struct
  open Owl
  module Tensor = Tensor

  module Vector = struct
    type t = int array [@@deriving compare, equal, sexp]

    let hash_fold_t = Hash.Builtin.hash_fold_array_frozen [%hash_fold: int]
  end

  module T = struct
    type t = Tensor of Tensor.t | Vector of Vector.t | Int of int | Error
    [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let eval _ op args =
    match (op, args) with
    | Op.Id t, [] -> Tensor t
    | Reshape, [ Tensor m; Vector v ] -> (
        try Tensor (Arr.reshape m v) with Invalid_argument _ -> Error)
    | Permute, [ Tensor m; Vector v ] -> Tensor (Arr.transpose ~axis:v m)
    | Flip, [ Tensor m; Int x ] -> Tensor (Arr.flip ~axis:x m)
    | Cons, [ Int x; Vector xs ] ->
        Vector (Array.of_list (x :: Array.to_list xs))
    | Vec, [ Int x; Int x' ] -> Vector [| x; x' |]
    | Int x, [] -> Int x
    | (Reshape | Permute | Flip), ([ Error; _ ] | [ _; Error ]) -> Error
    | op, args ->
        raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)]

  let dist _ _ _ = Float.infinity
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

  let _ =
    (Spec.add spec @@ Param.const_str ~name:"lang" name
      : (string, Param.bound) Param.t)

  let bench =
    Spec.add spec @@ Param.create @@ Bench.param (module Op) (module Value)
end

module Bench = Bench0

module Gen = struct
  include struct
    open Dumb_params

    let spec = Spec.create ()

    let ops =
      Spec.add spec
      @@ Param.ids ~name:"ops" ~doc:" operators to generate"
           (module Op)
           [ ("reshape", Reshape); ("flip", Flip); ("permute", Permute) ]

    let min_dims =
      Spec.add spec
      @@ Param.int ~name:"min-dims" ~doc:" min number of dimensions"
           ~init:(`Cli (Some 1)) ()

    let max_dims =
      Spec.add spec
      @@ Param.int ~name:"max-dims" ~doc:" max number of dimensions"
           ~init:(`Cli (Some 3)) ()

    let min_len =
      Spec.add spec
      @@ Param.int ~name:"min-len" ~doc:" min dimension length"
           ~init:(`Cli (Some 1)) ()

    let max_len =
      Spec.add spec
      @@ Param.int ~name:"max-len" ~doc:" max dimension length"
           ~init:(`Cli (Some 5)) ()
  end

  let factors n =
    List.init n ~f:(fun i -> i + 1) |> List.filter ~f:(fun m -> n mod m = 0)

  let random_ops params =
    let min_dims = Params.get params min_dims
    and max_dims = Params.get params max_dims
    and min_len = Params.get params min_len
    and max_len = Params.get params max_len in
    let dims = Random.int_incl min_dims max_dims in
    let dim_lens =
      Array.init dims ~f:(fun _ -> Random.int_incl min_len max_len)
    in
    let tensor =
      Owl.Arr.init_nd dim_lens (fun _ -> Float.of_int @@ Random.int 10)
    in
    let ints =
      List.init dims ~f:Fun.id @ factors (Array.fold ~f:( * ) ~init:1 dim_lens)
      |> List.dedup_and_sort ~compare
      |> List.map ~f:(fun i -> Op.Int i)
    in
    ints @ [ Op.Cons; Vec; Id tensor ] @ Params.get params ops

  let to_bench _ ops solution output = Bench.create ~solution ops output

  let check _ _ = true
end
