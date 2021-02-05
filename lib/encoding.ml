open Search_state
open Ast

module Path = struct
  module T = struct
    type t = int list [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let to_string p = List.map p ~f:Int.to_string |> String.concat ~sep:"."
end

module Eq = struct
  module T = struct
    type t = Path.t * Args.t [@@deriving compare, hash, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module Domains = struct
  type t = Set.M(Args).t Map.M(Path).t

  let empty = Map.empty (module Path)
end

module Domain_var = struct
  type t = Smt.Var.t Map.M(Args).t

  let create dom =
    let open Smt.Let_syntax in
    let%bind kv =
      Set.to_list dom
      |> List.map ~f:(fun k ->
             let%bind v = Smt.fresh_decl () in
             return (k, v))
      |> Smt.all
    in
    let _, vs = List.unzip kv in
    let%bind () = Smt.assert_ (Smt.exactly_one @@ List.map ~f:Smt.var vs) in
    return @@ Map.of_alist_exn (module Args) kv

  let eq v x = Map.find_exn v x |> Smt.var

  let case_split x f =
    let open Smt.Let_syntax in
    let%bind cases =
      Map.to_alist x
      |> List.map ~f:(fun (k, v) ->
             let%map y = f k in
             Smt.(var v => y))
      |> Smt.all
    in
    return @@ Smt.and_ cases
end

module Var = struct
  type t = { value : Symb.t Map.M(Type).t; op : Domain_var.t }
end

module App = struct
  module T = struct
    type t = Offset.t Op.t * Path.t list [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

module Ctx = struct
  type t = {
    vars : Var.t Map.M(Path).t;
    domains : Domains.t;  (** Domain of Args for each path. *)
    path_constrs : Domains.t Map.M(Eq).t;
        (** Domain restrictions implied by choosing an Args for a path *)
  }

  let empty =
    {
      vars = Map.empty (module Path);
      domains = Domains.empty;
      path_constrs = Map.empty (module Eq);
    }
end

let all_paths g target =
  let module Key = struct
    type t = State.t * Path.t [@@deriving compare, hash, sexp_of]
  end in
  let paths = Hash_set.create (module Key) in
  let q = Queue.create () in
  let rec loop () =
    match Queue.dequeue q with
    | Some ((state_v, path) as p) ->
        if Result.is_ok (Hash_set.strict_add paths p) then
          G.iter_succ
            (G.iter_succ_e
               (fun (_, i, v) ->
                 Queue.enqueue q (Node.to_state_exn v, path @ [ i ]))
               g)
            g (Node.of_state state_v);
        loop ()
    | None -> ()
  in
  Queue.enqueue q (target, []);
  loop ();
  Hash_set.to_list paths

let extend_context g ctx state_v path =
  let open Ctx in
  G.fold_succ
    (fun v ctx ->
      let args_v = Node.to_args_exn v in

      (* Update operator domains *)
      let domains =
        Map.update ctx.domains
          ~f:(fun dom ->
            let dom = Option.value dom ~default:(Set.empty (module Args)) in
            Set.add dom args_v)
          path
      (* Path constraints implied by the current choice of operator *)
      and new_path_constrs =
        G.succ_e g v
        |> List.map ~f:(fun (_, i, v') ->
               let path' = path @ [ i ] in
               let args =
                 G.succ g v'
                 |> List.map ~f:Node.to_args_exn
                 |> Set.of_list (module Args)
               in
               (path', args))
        |> Map.of_alist_exn (module Path)
      in

      let path_constrs =
        Map.update ctx.path_constrs (path, args_v) ~f:(function
          | Some old_path_constrs ->
              Map.merge old_path_constrs new_path_constrs ~f:(fun ~key ->
                function
                | `Both (x, x') -> Some (Set.union x x')
                | `Left x | `Right x -> Some x)
          | None -> new_path_constrs)
      in

      { ctx with domains; path_constrs })
    g (Node.of_state state_v) ctx

let build_context ss target =
  let open Smt.Let_syntax in
  let g = graph ss in
  let ctx =
    List.fold (all_paths g target) ~init:Ctx.empty ~f:(fun ctx (p, v) ->
        extend_context g ctx p v)
  in
  let%bind vars_alist =
    Map.to_alist ctx.domains
    |> List.map ~f:(fun (path, args) ->
           let open Symb in
           let%bind op = Domain_var.create args in
           let%bind value =
             let%map alist =
               Set.to_list args
               |> List.map ~f:(fun v -> Args.op ss v |> Op.ret_type)
               |> List.dedup_and_sort ~compare:[%compare: Type.t]
               |> List.map ~f:(fun t ->
                      let%map v = Symb.create (params ss) t in
                      (t, v))
               |> Smt.all
             in
             Map.of_alist_exn (module Type) alist
           in
           return (path, Var.{ op; value }))
    |> Smt.all
  in
  return { ctx with vars = Map.of_alist_exn (module Path) vars_alist }

let op_var ctx path = (Map.find_exn ctx.Ctx.vars path).Var.op

let symb_var ctx path type_ =
  Map.find_exn (Map.find_exn ctx.Ctx.vars path).Var.value type_

let domain_constr ctx dom =
  Map.to_alist dom
  |> List.map ~f:(fun (path, args) ->
         Set.to_list args
         |> List.map ~f:(fun arg -> Domain_var.eq (op_var ctx path) arg)
         |> Smt.or_)
  |> Smt.and_

let assert_path_constr ctx =
  Map.to_alist ctx.Ctx.path_constrs
  |> List.map ~f:(fun ((path, args_v), dom) ->
         Smt.(
           assert_
           @@ (Domain_var.eq (op_var ctx path) args_v => domain_constr ctx dom)))
  |> Smt.all_unit

let assert_value_constr ss ctx =
  let open Smt.Let_syntax in
  Map.to_alist ctx.Ctx.domains
  |> List.map ~f:(fun (path, _) ->
         let%bind cases =
           Domain_var.case_split (op_var ctx path) (fun args_v ->
               let op = Args.op ss args_v in
               let args_types, ret_type = Op.type_ op in
               let ret_var = symb_var ctx path ret_type
               and args_vars =
                 List.mapi args_types ~f:(fun i t ->
                     symb_var ctx (path @ [ i ]) t)
               in
               let%bind ret_value = Symb.eval (params ss) op args_vars in
               return Symb.(ret_var = ret_value))
         in
         Smt.assert_ cases)
  |> Smt.all_unit

let encode ss target =
  let open Smt.Let_syntax in
  let smt =
    let%bind ctx = build_context ss target in
    let%bind () = assert_path_constr ctx in
    let%bind () = assert_value_constr ss ctx in
    Smt.smtlib
  in
  Smt.eval smt

let[@landmark "find-program"] find_program ss target =
  let open Smt.Let_syntax in
  let string_of_args v =
    let op = Args.op ss v and id = Args.id v in
    [%string "%{op#Op}/%{id#Int}"]
  in

  let op_domains = Hashtbl.create (module Path) in
  let constrs = ref [] in

  let rec walk state_v path =
    G.succ (graph ss) (Node.of_state state_v)
    |> List.iter ~f:(fun v ->
           let args_v = Node.to_args_exn v in

           Hashtbl.update op_domains
             ~f:(function
               | Some dom -> Set.add dom args_v
               | None -> Set.singleton (module Args) args_v)
             path;

           let cases =
             G.succ_e (graph ss) v
             |> List.filter_map ~f:(fun (_, i, v') ->
                    let path' = path @ [ i ] in
                    let succ = G.succ (graph ss) v' in
                    if List.is_empty succ then None
                    else
                      List.map succ ~f:(fun v'' ->
                          let arg_str =
                            Node.to_args_exn v'' |> string_of_args
                          in
                          [%string "op(%{path'#Path}) = %{arg_str}"])
                      |> String.concat ~sep:" || " |> sprintf "(%s)"
                      |> Option.return)
           in
           ( if not (List.is_empty cases) then
             let arg_str = string_of_args args_v in
             let cases_str = String.concat cases ~sep:" && " in

             constrs :=
               [%string "op(%{path#Path}) = %{arg_str} => %{cases_str}"]
               :: !constrs );

           G.iter_succ_e
             (fun (_, i, v') -> walk (Node.to_state_exn v') (path @ [ i ]))
             (graph ss) v)
  in

  (match target with v :: _ -> walk v [] | _ -> assert false);

  let module OOp = struct
    module T = struct
      type t = Offset.t Op.t [@@deriving compare, sexp]
    end

    include T
    include Comparator.Make (T)
  end in
  Hashtbl.iteri op_domains ~f:(fun ~key:path ~data ->
      print_endline
        ( Set.to_list data
        |> List.map ~f:(fun v ->
               let v_str = string_of_args v in
               [%string "op(%{path#Path}) = %{v_str}"])
        |> String.concat ~sep:" || " );

      Set.to_list data
      |> List.map ~f:(fun v -> (Args.op ss v, v))
      |> Map.of_alist_multi (module OOp)
      |> Map.iteri ~f:(fun ~key:op ~data ->
             let lhs =
               List.map data ~f:(fun v ->
                   let v_str = string_of_args v in
                   [%string "op(%{path#Path}) = %{v_str}"])
               |> String.concat ~sep:" || " |> sprintf "(%s)"
             in
             let args =
               List.init (Op.arity op) ~f:(fun i -> path @ [ i ])
               |> List.map ~f:Path.to_string |> String.concat ~sep:", "
             in
             let rhs = [%string "var(%{path#Path}) = %{op#Op}(%{args})"] in
             print_endline [%string "%{lhs} => %{rhs}"]));
  List.iter !constrs ~f:print_endline
