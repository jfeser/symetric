module Make (Lang_op : Op_intf.S) = struct
  module Type = struct
    type t = Term
  end

  module Op = struct
    module T = struct
      type t =
        | Rename of { node : int; op : Lang_op.t }
        | Insert of { node : int; children : int list; op : Lang_op.t }
        | Delete of { node : int; child : int }
        | Id of Lang_op.t Program.t
      [@@deriving compare, sexp]
    end

    include T
    include Comparator.Make (T)

    let arity = function Rename _ | Insert _ | Delete _ -> 1 | Id _ -> 0
  end

  module P = Program

  module Value = struct
    type t = Lang_op.t Program.t [@@deriving compare, hash, sexp]

    let replace ?order p i p' =
      let pa = P.annotate ?order p in
      let rec replace (P.Apply ((j, op), args)) =
        if i = j then p' else P.apply op ~args:(List.map ~f:replace args)
      in
      replace pa

    let unannotate = P.map ~f:(fun (_, op) -> op)

    let nth_annot ?order p i =
      let pa = P.annotate ?order p in
      let rec nth (P.Apply ((j, _), args) as p') =
        if i = j then Some p' else List.find_map args ~f:nth
      in
      nth pa

    let nth ?order p i = nth_annot ?order p i |> Option.map ~f:unannotate

    let eval _ op args =
      match (op, args) with
      | Op.Id p, [] -> p
      | Rename x, [ t ] ->
          P.mapi ~f:(fun i op -> if i = x.node then x.op else op) t
      | Insert x, [ t ] ->
          let args =
            List.map x.children ~f:(fun i ->
                Option.value_exn (nth ~order:`Post t i))
          in
          replace t x.node (P.apply x.op ~args)
      | Delete x, [ t ] ->
          let rec delete (P.Apply ((id, op), args)) =
            if id = x.node then List.nth_exn args x.child
            else P.apply (id, op) ~args:(List.map args ~f:delete)
          in
          P.annotate t |> delete |> unannotate
      | op, args ->
          raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)]
  end

  type ctx = {
    ops : Lang_op.t list;
    ops_by_arity : Lang_op.t list Hashtbl.M(Int).t;
  }

  let sample_id v = Random.int (Program.size v)

  let sample_op_arity ctx arity =
    Hashtbl.find_exn ctx.ops_by_arity arity |> List.random_element_exn

  let sample_op ctx = List.random_element_exn ctx.ops

  let sample_rename ctx v =
    let node = sample_id v in
    let (Apply (old_op, _)) = Option.value_exn (Value.nth v node) in
    let arity = Lang_op.arity old_op in
    Op.Rename { node; op = sample_op_arity ctx arity }

  let sample_insert ctx v =
    let new_op = sample_op ctx in
    let children = List.init (Lang_op.arity new_op) ~f:(fun _ -> sample_id v) in
    Op.Insert { node = sample_id v; op = new_op; children }

  let sample_delete v =
    let rec nodes_with_children (Program.Apply (op, args)) =
      if List.is_empty args then []
      else (op, args) :: List.concat_map args ~f:nodes_with_children
    in
    nodes_with_children @@ Program.annotate v
    |> List.random_element
    |> Option.map ~f:(fun ((node, _), args) ->
           let child = Random.int @@ List.length args in
           Op.Delete { node; child })

  let rec sample_op ctx v =
    let op_kind = List.random_element_exn [ `Rename; `Insert; `Delete ] in
    let op =
      match op_kind with
      | `Rename -> Some (sample_rename ctx v)
      | `Insert -> Some (sample_insert ctx v)
      | `Delete -> sample_delete v
    in
    match op with Some op -> op | None -> sample_op ctx v

  let rec sample_program_and_value ctx n v =
    if n <= 1 then (P.apply (Op.Id v), v)
    else
      let p, v = sample_program_and_value ctx (n - 1) v in
      let op = sample_op ctx v in
      let v' = Value.eval () op [ v ] in
      (Apply (op, [ p ]), v')

  let mk_ctx ops =
    {
      ops;
      ops_by_arity =
        Hashtbl.of_alist_multi (module Int)
        @@ List.map ops ~f:(fun op -> (Lang_op.arity op, op));
    }

  let sample ops n v =
    let ctx = mk_ctx ops in
    sample_program_and_value ctx n v

  let sample_single ops =
    let ctx = mk_ctx ops in
    fun v -> sample_op ctx v
end
