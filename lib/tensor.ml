include (
  struct
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

      let compare a a' =
        if equal a a' then 0 else if M.greater a a' then 1 else -1

      let sexp_of_t a =
        [%sexp_of: float array * int array] (M.to_array a, M.shape a)

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
            raise_s
              [%message "unexpected arguments" (op : Op.t) (args : t list)]

      let dist _ _ _ = Float.infinity
    end

    module Bench0 = struct
      type t = (Op.t, Value.t) Bench.t [@@deriving compare, sexp]

      let solution_exn = Bench.solution_exn

      let output = Bench.output

      let ops = Bench.ops
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
  end :
    Lang_intf.S)
