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
end

module Value = struct
  open Owl
  module Tensor = Tensor

  module Vector = struct
    type t = int list [@@deriving compare, hash, equal, sexp]
  end

  module T = struct
    type t = Tensor of Tensor.t | Vector of Vector.t | Int of int | Error [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let eval _ op args =
    match (op, args) with
    | Op.Id t, [] -> Tensor t
    | Reshape, [ Tensor m; Vector v ] -> (
        try Tensor (Arr.reshape m @@ Array.of_list v) with Invalid_argument _ -> Error)
    | Permute, [ Tensor m; Vector v ] ->
        let dims = Arr.num_dims m in
        if List.length v <> dims then Error
        else
          let found = Array.create ~len:dims false in
          List.iter v ~f:(fun ax -> if ax >= 0 && ax < dims then found.(ax) <- true);
          if Array.for_all ~f:Fun.id found then Tensor (Arr.transpose ~axis:(Array.of_list v) m) else Error
    | Flip, [ Tensor m; Int x ] ->
        let dims = Arr.num_dims m in
        if x >= 0 && x < dims then Tensor (Arr.flip ~axis:x m) else Error
    | Cons, [ Int x; Vector xs ] -> Vector (x :: xs)
    | Vec, [ Int x ] -> Vector [ x ]
    | Int x, [] -> Int x
    | (Reshape | Permute | Flip), ([ Error; _ ] | [ _; Error ]) -> Error
    | op, args -> raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)]

  let dist _ v v' =
    match (v, v') with
    | Tensor t, Tensor t' -> if Owl.Arr.num_dims t <> Owl.Arr.num_dims t' then 1.0 else 0.0
    | _ -> Float.infinity
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
    let dim_lens = Array.init dims ~f:(fun _ -> Random.int_incl min_len max_len) in

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
          let new_shape = List.random_element_exn (k_factors (Owl.Arr.numel t) new_dims) in
          Program.apply Op.Reshape ~args:[ p; Op.vec_of_list new_shape ]
      | `Permute ->
          let dims =
            List.init (Owl.Arr.num_dims t) ~f:Fun.id
            |> Combinat.permutations |> Combinat.random |> List.hd_exn |> Array.to_list
          in
          Program.apply Op.Permute ~args:[ p; Op.vec_of_list dims ]
      | `Flip ->
          let axis = Random.int_incl 0 @@ (Owl.Arr.num_dims t - 1) in
          Program.(apply Op.Flip ~args:[ p; apply (Op.Int axis) ])
    in

    let generate n =
      let tensor = Owl.Arr.init_nd dim_lens (fun _ -> Float.of_int @@ Random.int 10) in
      let p_init = Program.apply (Op.Id tensor) in
      let rec add_ops p = if Program.size p < n then add_ops @@ wrap p else p in
      add_ops p_init
    in

    let p_final = generate size in
    if Program.size p_final = size && check params p_final then Some (p_final, Program.ops p_final) else None

  let to_bench _ ops solution output = Bench.create ~solution ops output
end
