open Search_state
open Ast

module Path = struct
  module T = struct
    type t = int list [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let to_string p = List.map p ~f:Int.to_string |> String.concat ~sep:"."

  let pp fmt p = Fmt.pf fmt "%s" @@ to_string p
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

  let create ?prefix dom =
    let open Smt.Let_syntax in
    let%bind kv =
      Set.to_list dom
      |> List.map ~f:(fun k ->
             let%bind v = Smt.fresh_decl ?prefix () in
             return (k, v))
      |> Smt.all
    in
    let _, vs = List.unzip kv in
    let%bind () = Smt.assert_ (Smt.exactly_one @@ List.map ~f:Smt.var vs) in
    return @@ Map.of_alist_exn (module Args) kv

  let eq v x = Map.find_exn v x |> Smt.var

  let cases = Map.to_alist
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
  List.iter target ~f:(fun t -> Queue.enqueue q (t, []));
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
              Map.merge old_path_constrs new_path_constrs ~f:(fun ~key:_ ->
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
           let%bind op =
             Domain_var.create ~prefix:(Fmt.str "op_%a_" Path.pp path) args
           in
           let%bind value =
             let%map alist =
               Set.to_list args
               |> List.map ~f:(fun v -> Args.op ss v |> Op.ret_type)
               |> List.dedup_and_sort ~compare:[%compare: Type.t]
               |> List.map ~f:(fun t ->
                      let%map v =
                        Symb.create
                          ~prefix:(Fmt.str "var_%a_b%d_" Path.pp path)
                          (params ss) t
                      in
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

let paths ctx = Map.keys ctx.Ctx.domains

let assert_value_constr ss ctx =
  let open Smt.Let_syntax in
  List.fold (paths ctx)
    ~init:(return @@ Map.empty (module App))
    ~f:(fun app_cache path ->
      Domain_var.cases (op_var ctx path)
      |> List.fold ~init:app_cache ~f:(fun app_cache (args_v, control_var) ->
             let%bind app_cache = app_cache in
             let op = Args.op ss args_v in
             let app =
               let args = List.init (Op.arity op) ~f:(fun i -> path @ [ i ]) in
               (op, args)
             in
             let%bind ret_value, app_cache =
               match Map.find app_cache app with
               | Some v -> return (v, app_cache)
               | None ->
                   let%bind ret_value =
                     let args_types, _ = Op.type_ op in
                     let args_vars =
                       List.mapi args_types ~f:(fun i t ->
                           symb_var ctx (path @ [ i ]) t)
                     in

                     Symb.eval (params ss) op args_vars
                   in
                   return (ret_value, Map.set app_cache ~key:app ~data:ret_value)
             in

             let ret_var =
               let ret_type = Op.ret_type op in
               symb_var ctx path ret_type
             in

             let%bind () =
               Smt.assert_ Smt.(var control_var => Symb.(ret_var = ret_value))
             in
             return app_cache))
  |> Smt.ignore_m

let check ss target =
  let open Smt.Let_syntax in
  let smt =
    let%bind ctx = build_context ss target in
    let%bind () =
      let expected =
        Conc.bool_vector (params ss).bench.output
        |> Symb.of_conc (params ss).offsets
      in

      Smt.assert_
        ( List.map target ~f:(fun v ->
              Symb.(expected = symb_var ctx [] (State.type_ ss v)))
        |> Smt.or_ )
    in
    let%bind () = assert_path_constr ctx in
    let%bind () = assert_value_constr ss ctx in
    Smt.check_sat
  in
  Smt.eval smt

let[@landmark "find-program"] find_program ss target =
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
