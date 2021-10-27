module type OP = sig
  type t [@@deriving compare, hash, sexp]
end

module type VALUE = sig
  type op

  type t [@@deriving compare, hash, sexp]

  module Ctx : sig
    type t
  end

  val eval : Ctx.t -> op -> t list -> t

  val is_error : t -> bool
end

module Make (Op : OP) (Value : VALUE with type op := Op.t) = struct
  include Hash_cached.Make_sexp_of (Value)

  module App = struct
    module T = struct
      type nonrec t = Op.t * t list [@@deriving compare, hash, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end

  module Ctx = struct
    type nonrec t = { ctx : Value.Ctx.t; tbl : t Hashtbl.M(App).t }

    let of_ctx ctx = { ctx; tbl = Hashtbl.create (module App) }
  end

  let eval (ctx : Ctx.t) op args =
    match Hashtbl.find ctx.tbl (op, args) with
    | Some v -> v
    | None ->
        let v = create @@ Value.eval ctx.ctx op @@ List.map ~f:value args in
        Hashtbl.set ctx.tbl ~key:(op, args) ~data:v;
        v

  let is_error v = Value.is_error @@ value v
end

module Make_cached (Op : OP) (Value : VALUE with type op := Op.t) = struct
  include Hash_cached.Make (Value)
  module Ctx = Value.Ctx

  let eval ctx op args = create @@ Value.eval ctx op @@ List.map ~f:value args

  let is_error v = Value.is_error @@ value v
end
