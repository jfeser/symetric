module Make (Lang : Lang_intf.S) = struct
  open Lang

  type t = {
    params : Lang.params;
    of_cost : Value.t Queue.t array;
    paths : (Op.t * Value.t list) Hashtbl.M(Value).t;
  }

  let create params =
    {
      params;
      of_cost = Array.init (params.max_cost + 1) ~f:(fun _ -> Queue.create ());
      paths = Hashtbl.create (module Value);
    }

  let params ctx = ctx.params

  let of_cost ctx c = Queue.to_list ctx.of_cost.(c)

  let mem ctx = Hashtbl.mem ctx.paths

  let insert ctx cost state op inputs =
    if not (mem ctx state) then (
      Queue.enqueue ctx.of_cost.(cost) state;
      Hashtbl.set ctx.paths ~key:state ~data:(op, inputs))

  let states ctx = Hashtbl.keys ctx.paths

  let length ctx = Hashtbl.length ctx.paths

  let print_stats ctx =
    Array.iteri ctx.of_cost ~f:(fun i q ->
        Fmt.epr "Cost %d: %d\n" i @@ Queue.length q);
    Fmt.epr "Total: %d\n%!" (length ctx)

  let rec program_exn ctx state =
    let op, args = Hashtbl.find_exn ctx.paths state in
    Program.Apply (op, List.map args ~f:(program_exn ctx))
end
