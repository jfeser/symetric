module type OP = sig
  type t [@@deriving compare, hash, sexp]
end

module type VALUE = sig
  type op

  type t [@@deriving compare, equal, hash, sexp_of]

  module Ctx : sig
    type t

    val of_params : Params.t -> t
  end

  include Comparator.S with type t := t

  val eval : Ctx.t -> op -> t list -> t
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
end

module Make_cached (Op : OP) (Value : VALUE with type op := Op.t) = struct
  include Hash_cached.Make_sexp_of (Value)
  module Ctx = Value.Ctx

  let eval ctx op args = create @@ Value.eval ctx op @@ List.map ~f:value args
end
