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
    if List.fold ~f:( * ) ~init:1 shape = n_elems t then Some (of_owl @@ T.reshape (to_owl t) (Array.of_list shape))
    else None

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

  let cost = function Cons -> 2 | _ -> 1
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

  module Ctx = struct
    type t = unit

    let of_params _ = ()
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

module Gen = struct
  include struct
    open Dumb_params

    let spec = Spec.create ()

    let ops =
      Spec.add spec
      @@ Param.ids ~name:"ops" ~doc:" operators to generate"
           (module Op)
           [ ("reshape", Reshape); ("flip", Flip); ("permute", Permute) ]

    let min_dims = Spec.add spec @@ Param.int ~name:"min-dims" ~doc:" min number of dimensions" ~init:(`Cli (Some 1)) ()

    let max_dims = Spec.add spec @@ Param.int ~name:"max-dims" ~doc:" max number of dimensions" ~init:(`Cli (Some 3)) ()

    let min_len = Spec.add spec @@ Param.int ~name:"min-len" ~doc:" min dimension length" ~init:(`Cli (Some 1)) ()

    let max_len = Spec.add spec @@ Param.int ~name:"max-len" ~doc:" max dimension length" ~init:(`Cli (Some 5)) ()
  end

  let factors n = List.init n ~f:(fun i -> i + 1) |> List.filter ~f:(fun m -> n mod m = 0)

  let rec k_factors n k =
    if k = 1 then [ [ n ] ]
    else
      List.init n ~f:(fun i -> i + 1)
      |> List.filter ~f:(fun m -> n mod m = 0)
      |> List.concat_map ~f:(fun m -> List.map (k_factors (n / m) (k - 1)) ~f:(fun ms -> m :: ms))

  let%expect_test "" =
    print_s [%message (k_factors 16 1 : int list list)];
    print_s [%message (k_factors 16 2 : int list list)];
    print_s [%message (k_factors 16 3 : int list list)];
    print_s [%message (k_factors 16 4 : int list list)];
    [%expect
      {|
      ("k_factors 16 1" ((16)))
      ("k_factors 16 2" ((1 16) (2 8) (4 4) (8 2) (16 1)))
      ("k_factors 16 3"
       ((1 1 16) (1 2 8) (1 4 4) (1 8 2) (1 16 1) (2 1 8) (2 2 4) (2 4 2) (2 8 1)
        (4 1 4) (4 2 2) (4 4 1) (8 1 2) (8 2 1) (16 1 1)))
      ("k_factors 16 4"
       ((1 1 1 16) (1 1 2 8) (1 1 4 4) (1 1 8 2) (1 1 16 1) (1 2 1 8) (1 2 2 4)
        (1 2 4 2) (1 2 8 1) (1 4 1 4) (1 4 2 2) (1 4 4 1) (1 8 1 2) (1 8 2 1)
        (1 16 1 1) (2 1 1 8) (2 1 2 4) (2 1 4 2) (2 1 8 1) (2 2 1 4) (2 2 2 2)
        (2 2 4 1) (2 4 1 2) (2 4 2 1) (2 8 1 1) (4 1 1 4) (4 1 2 2) (4 1 4 1)
        (4 2 1 2) (4 2 2 1) (4 4 1 1) (8 1 1 2) (8 1 2 1) (8 2 1 1) (16 1 1 1))) |}]

  let check params p = match Program.eval (Value.eval params) p with Error -> false | _ -> true

  let random_program params size =
    let min_dims = Params.get params min_dims
    and max_dims = Params.get params max_dims
    and min_len = Params.get params min_len
    and max_len = Params.get params max_len in
    let dims = Random.int_incl min_dims max_dims in
    let shape = List.init dims ~f:(fun _ -> Random.int_incl min_len max_len) in

    let wrap p =
      let t =
        match Program.eval (Value.eval params) p with
        | Tensor t -> t
        | v -> raise_s [%message "not a tensor" (p : Op.t Program.t) (v : Value.t)]
      in
      let op = List.random_element_exn [ `Reshape; `Permute; `Flip ] in
      match op with
      | `Reshape ->
          let new_dims = Random.int_incl min_dims max_dims in
          let new_shape = List.random_element_exn (k_factors (Tensor.n_elems t) new_dims) in
          Program.apply Op.Reshape ~args:[ p; Op.vec_of_list new_shape ]
      | `Permute ->
          let dims =
            List.init (Tensor.n_dims t) ~f:Fun.id
            |> Combinat.permutations |> Combinat.random |> List.hd_exn |> Array.to_list
          in
          Program.apply Op.Permute ~args:[ p; Op.vec_of_list dims ]
      | `Flip ->
          let axis = Random.int_incl 0 @@ (Tensor.n_dims t - 1) in
          Program.(apply Op.Flip ~args:[ p; apply (Op.Int axis) ])
    in

    let generate n =
      let n_elems = List.reduce_exn shape ~f:( * ) in
      let (tensor : Tensor.t) = { elems = List.init n_elems ~f:(fun _ -> Random.int 10); shape } in
      let p_init = Program.apply (Op.Id tensor) in
      let rec add_ops p = if Program.size p < n then add_ops @@ wrap p else p in
      add_ops p_init
    in

    let p_final = generate size in
    if Program.size p_final = size && check params p_final then Some (p_final, Program.ops p_final) else None

  let to_bench _ ops solution output = Bench.create ~solution ops output
end
