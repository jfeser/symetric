module Make (Lang : Lang_intf.S) = struct
  module Attr = struct
    module T = struct
      type t = { cost : int; type_ : Lang.Type.t } [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  type t = {
    max_cost : int;
    values : Lang.Value.t Queue.t Hashtbl.M(Attr).t;
    paths : (Lang.Op.t * Lang.Value.t list) Queue.t Hashtbl.M(Lang.Value).t;
  }

  let create max_cost = { max_cost; values = Hashtbl.create (module Attr); paths = Hashtbl.create (module Lang.Value) }

  let search ctx ~cost ~type_ =
    if cost >= 0 && cost <= ctx.max_cost then
      match Hashtbl.find ctx.values { cost; type_ } with Some q -> Queue.to_list q | None -> []
    else []

  let mem ctx = Hashtbl.mem ctx.paths

  let insert ctx cost state op inputs =
    if not (mem ctx state) then (
      let type_ = Lang.Op.ret_type op in
      let q = Hashtbl.find_or_add ctx.values { cost; type_ } ~default:Queue.create in
      Queue.enqueue q state;

      let paths = Hashtbl.find_or_add ctx.paths state ~default:Queue.create in
      Queue.enqueue paths (op, inputs))

  let states ctx = Hashtbl.keys ctx.paths

  let length ctx = Hashtbl.length ctx.paths

  let print_stats ctx = Fmt.epr "Total: %d\n%!" (length ctx)

  let rec program_exn ctx state =
    let op, args = Queue.peek_exn @@ Hashtbl.find_exn ctx.paths state in
    Program.Apply (op, List.map args ~f:(program_exn ctx))

  let program_of_op_args_exn ctx op args = Program.Apply (op, List.map args ~f:(program_exn ctx))

  let clear { values; paths } =
    Hashtbl.clear values;
    Hashtbl.clear paths

  let cost_of ctx v =
    Hashtbl.to_alist ctx.values
    |> List.find_map ~f:(fun (k, vs) ->
           if Queue.mem vs v ~equal:[%compare.equal: Lang.Value.t] then Some k.cost else None)
end
