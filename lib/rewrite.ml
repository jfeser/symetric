module Make (Lang : Lang_intf.S) = struct
  module Type = struct
    type t = Term
  end

  module Op = struct
    module T = struct
      type t =
        | Rename of { node : int; op : Lang.Op.t }
        | Insert of { node : int; children : int list; op : Lang.Op.t }
        | Delete of { node : int; child : int }
        | Id of Lang.Op.t Program.t
      [@@deriving compare, sexp]
    end

    include T
    include Comparator.Make (T)

    let arity = function Rename _ | Insert _ | Delete _ -> 1 | Id _ -> 0
  end

  module P = Program

  module Value = struct
    type t = Lang.Op.t Program.t [@@deriving compare, hash, sexp]

    let replace ~order p i p' =
      let pa = P.annotate ~order p in
      let rec replace (P.Apply ((j, op), args)) =
        if i = j then p' else P.apply op ~args:(List.map ~f:replace args)
      in
      replace pa

    let unannotate = P.map ~f:(fun (_, op) -> op)

    let nth_annot ~order p i =
      let pa = P.annotate ~order p in
      let rec nth (P.Apply ((j, _), args) as p') =
        if i = j then Some p' else List.find_map args ~f:nth
      in
      nth pa

    let nth ~order p i = nth_annot ~order p i |> Option.map ~f:unannotate

    let eval _ op args =
      match (op, args) with
      | Op.Id p, [] -> p
      | Rename x, [ t ] ->
          P.mapi ~order:`Post ~f:(fun i op -> if i = x.node then x.op else op) t
      | Insert x, [ t ] ->
          let args =
            List.map x.children ~f:(fun i ->
                Option.value_exn (nth ~order:`Post t i))
          in
          replace ~order:`Post t x.node (P.apply x.op ~args)
      | Delete x, [ t ] ->
          let rec delete (P.Apply ((id, op), args)) =
            if id = x.node then List.nth_exn args x.child
            else P.apply (id, op) ~args:(List.map args ~f:delete)
          in
          P.annotate ~order:`Post t |> delete |> unannotate
      | op, args ->
          raise_s [%message "unexpected arguments" (op : Op.t) (args : t list)]
  end

  let sample ops n p =
    let ops_by_arity =
      Hashtbl.of_alist_multi (module Int)
      @@ List.map ops ~f:(fun op -> (Lang.Op.arity op, op))
    in
    let sample_id v = Random.int (Program.size v) in
    let sample_op_arity arity =
      Hashtbl.find_exn ops_by_arity arity |> List.random_element_exn
    in
    let sample_op () = List.random_element_exn ops in
    let sample_rename v =
      let node = sample_id v in
      let (Apply (old_op, _)) =
        Option.value_exn (Value.nth ~order:`Post v node)
      in
      let arity = Lang.Op.arity old_op in
      Op.Rename { node; op = sample_op_arity arity }
    in
    let sample_insert v =
      let new_op = sample_op () in
      let children =
        List.init (Lang.Op.arity new_op) ~f:(fun _ -> sample_id v)
      in
      Op.Insert { node = sample_id v; op = new_op; children }
    in
    let sample_delete v =
      let node = sample_id v in
      let (Apply (_, all_children)) =
        Option.value_exn (Value.nth_annot ~order:`Post v node)
      in
      let (Apply ((child, _), _)) = List.permute all_children |> List.hd_exn in
      Op.Delete { node; child }
    in
    let sample_op v =
      let op_kind = List.random_element_exn [ `Rename; `Insert; `Delete ] in
      match op_kind with
      | `Rename -> sample_rename v
      | `Insert -> sample_insert v
      | `Delete -> sample_delete v
    in
    let rec sample_program n v =
      if n <= 1 then P.apply (Op.Id v)
      else
        let op = sample_op v in
        let v' = Value.eval () op [ v ] in
        Apply (op, [ sample_program (n - 1) v' ])
    in
    sample_program n p
end
