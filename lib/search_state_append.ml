module Make (Lang : Lang_intf.S) = struct
  open Lang

  module Attr = struct
    module T = struct
      type t = { cost : int; type_ : Type.t } [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  type t = {
    max_cost : int;
    values : Value.t Queue.t Hashtbl.M(Attr).t;
    paths : (Op.t * Value.t list) Hashtbl.M(Value).t;
  }

  let create max_cost =
    {
      max_cost;
      values = Hashtbl.create (module Attr);
      paths = Hashtbl.create (module Value);
    }

  let search ctx ~cost ~type_ =
    if cost >= 0 && cost <= ctx.max_cost then
      match Hashtbl.find ctx.values { cost; type_ } with
      | Some q -> Queue.to_list q
      | None -> []
    else []

  let mem ctx = Hashtbl.mem ctx.paths

  let insert ctx cost state op inputs =
    if not (mem ctx state) then (
      let type_ = Op.ret_type op in
      let q =
        Hashtbl.find_or_add ctx.values { cost; type_ } ~default:Queue.create
      in
      Queue.enqueue q state;
      Hashtbl.set ctx.paths ~key:state ~data:(op, inputs))

  let states ctx = Hashtbl.keys ctx.paths

  let length ctx = Hashtbl.length ctx.paths

  let print_stats ctx = Fmt.epr "Total: %d\n%!" (length ctx)

  let rec program_exn ctx state =
    let op, args = Hashtbl.find_exn ctx.paths state in
    Program.Apply (op, List.map args ~f:(program_exn ctx))

  let program_of_op_args_exn ctx op args =
    Program.Apply (op, List.map args ~f:(program_exn ctx))
end
