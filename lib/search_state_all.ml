module Make (Lang : Lang_intf.S) = struct
  module Attr = struct
    module T = struct
      type t = { cost : int; type_ : Lang.Type.t } [@@deriving compare, hash, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  module TValue = struct
    type t = { type_ : Lang.Type.t; value : Lang.Value.t } [@@deriving compare, hash, sexp_of]
  end

  type t = {
    max_cost : int;
    values : Lang.Value.t Queue.t Hashtbl.M(Attr).t;
    paths : (int * Lang.Op.t * Lang.Value.t list) Queue.t Hashtbl.M(TValue).t;
  }

  let create max_cost = { max_cost; values = Hashtbl.create (module Attr); paths = Hashtbl.create (module TValue) }

  let search ctx ~cost ~type_ =
    if cost >= 0 && cost <= ctx.max_cost then
      match Hashtbl.find ctx.values { cost; type_ } with Some q -> Queue.to_list q | None -> []
    else []

  let mem ctx = Hashtbl.mem ctx.paths

  let insert ctx cost state op inputs =
    let type_ = Lang.Op.ret_type op in
    let tvalue = TValue.{ type_; value = state } in
    (if not (mem ctx tvalue) then
     let q = Hashtbl.find_or_add ctx.values { cost; type_ } ~default:Queue.create in
     Queue.enqueue q state);

    let paths = Hashtbl.find_or_add ctx.paths tvalue ~default:Queue.create in
    Queue.enqueue paths (cost, op, inputs)

  let states ctx = Hashtbl.keys ctx.paths

  let length ctx = Hashtbl.length ctx.paths

  let print_stats ctx = Fmt.epr "Total: %d\n%!" (length ctx)

  let rec program_exn ctx type_ value =
    let _, op, args = Queue.peek_exn @@ Hashtbl.find_exn ctx.paths { type_; value } in
    program_of_op_args_exn ctx op args

  and program_of_op_args_exn ctx op args =
    Program.Apply (op, List.map2_exn (Lang.Op.args_type op) args ~f:(program_exn ctx))

  let rec random_program_exn ctx max_cost type_ value =
    let q = Hashtbl.find_exn ctx.paths { type_; value } |> Queue.filter ~f:(fun (c, _, _) -> c <= max_cost) in
    let c, op, args = Queue.get q (Random.int @@ Queue.length q) in
    Program.Apply (op, List.map2_exn (Lang.Op.args_type op) args ~f:(random_program_exn ctx (c - 1)))

  let random_program_exn ctx state = random_program_exn ctx Int.max_value state

  let clear { values; paths } =
    Hashtbl.clear values;
    Hashtbl.clear paths

  let cost_of ctx v =
    Hashtbl.to_alist ctx.values
    |> List.find_map ~f:(fun (k, vs) ->
           if Queue.mem vs v ~equal:[%compare.equal: Lang.Value.t] then Some k.cost else None)
end
