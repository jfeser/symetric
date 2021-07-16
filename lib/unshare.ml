open Graph_ext

module One_to_many = struct
  type ('a, 'b) t = { forward : 'a -> 'b Sequence.t; backward : 'b -> 'a }
end

let edge_relation vrel =
  let open One_to_many in
  let forward (v, x, v') =
    vrel.forward v |> Sequence.concat_map ~f:(fun w -> vrel.forward v' |> Sequence.map ~f:(fun w' -> (w, x, w')))
  in
  let backward (w, x, w') = (vrel.backward w, x, vrel.backward w') in
  { forward; backward }

module Make
    (G : LABELED_GRAPH with type label = int) (K : sig
      val kind : G.V.t -> [ `Args | `State ]
    end) =
struct
  module V_ref = struct
    module T = struct
      type t = { id : int; node : (G.V.t[@ignore]) } [@@deriving compare, equal, hash, sexp_of]
    end

    include T
    include Comparator.Make (T)

    let vertex x = x.node

    let pp pp_node fmt x = Fmt.pf fmt "%a@%d" pp_node x.node x.id
  end

  module E = struct
    include Int

    let default = -1
  end

  module G_replicated = Graph_ext.Make (V_ref) (E)
  (** Represents graphs that can contain multiple instances of the same
       vertex. *)

  module Port = struct
    module T = struct
      type t = { node : V_ref.t; port : int } [@@deriving compare, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end

  let id v = v.V_ref.id

  let unshare g =
    let vrefs = ref (Map.empty (module G.V)) in
    let create =
      let id_ctr = ref 0 in
      fun node ->
        incr id_ctr;
        let vref = V_ref.{ id = !id_ctr; node } in
        vrefs := Map.add_multi !vrefs ~key:node ~data:vref;
        vref
    in

    let g_replicated = G_replicated.create () in

    let work = Hash_queue.create { hash = G.V.hash; compare = G.V.compare; sexp_of_t = [%sexp_of: G.V.t] } in

    let update q key ~f =
      let data' = Hash_queue.lookup q key |> f in
      match Hash_queue.replace q key data' with `Ok -> () | `No_such_key -> Hash_queue.enqueue_back_exn q key data'
    in

    let add_ancestor ancestors port =
      match K.kind port.Port.node.node with `Args -> Set.add ancestors port | `State -> ancestors
    in

    let exists_common_ancestor =
      Set.fold_until
        ~init:(Map.empty (module V_ref))
        ~f:(fun ancestor_ports Port.{ node; port } ->
          match Map.find ancestor_ports node with
          | None -> Continue (Map.set ancestor_ports ~key:node ~data:port)
          | Some port' -> if port = port' then Continue ancestor_ports else Stop true)
        ~finish:(fun _ -> false)
    in

    let rec loop () =
      (* Fmt.pr "%a@."
       *   Fmt.(Dump.(list @@ list @@ pair Port.pp @@ list Port.pp))
       *   ( Hash_queue.to_list work
       *   |> List.map ~f:(List.map ~f:(fun (p, ps) -> (p, Set.to_list ps))) ); *)
      match Hash_queue.dequeue_front_with_key work with
      | Some (vertex, in_edges) ->
          let _, ancestors = List.unzip in_edges in
          let ancestors = Set.union_list (module Port) ancestors in
          let any_overlap = exists_common_ancestor ancestors in

          if List.is_empty in_edges then (
            let this_ref = create vertex in

            G_replicated.add_vertex g_replicated this_ref;

            G.succ_e g vertex
            |> List.iter ~f:(fun (_, idx', child) ->
                   let port_ref = Port.{ node = this_ref; port = idx' } in
                   let child_in_edge = (port_ref, add_ancestor (Set.empty (module Port)) port_ref) in
                   update work child ~f:(fun edges -> child_in_edge :: Option.value edges ~default:[])))
          else if not any_overlap then (
            let this_ref = create vertex in
            List.iter in_edges ~f:(fun (Port.{ node = parent; port = idx }, _) ->
                G_replicated.add_edge_e g_replicated (parent, idx, this_ref));

            G.succ_e g vertex
            |> List.iter ~f:(fun (_, idx', child) ->
                   let this_port = Port.{ node = this_ref; port = idx' } in
                   let child_in_edge = (this_port, add_ancestor ancestors this_port) in
                   update work child ~f:(fun edges -> child_in_edge :: Option.value edges ~default:[])))
          else
            List.iter in_edges ~f:(fun (Port.{ node = parent; port = idx }, ancestors) ->
                let this_ref = create vertex in

                G_replicated.add_edge_e g_replicated (parent, idx, this_ref);

                G.succ_e g vertex
                |> List.iter ~f:(fun (_, idx', child) ->
                       let this_port = Port.{ node = this_ref; port = idx' } in
                       let child_in_edge = (this_port, add_ancestor ancestors this_port) in
                       update work child ~f:(fun edges -> child_in_edge :: Option.value edges ~default:[])));
          loop ()
      | None -> ()
    in

    G.iter_vertex (fun v -> if G.in_degree g v = 0 then Hash_queue.enqueue_back_exn work v []) g;
    loop ();

    let rel =
      let forward v = Map.find_exn !vrefs v |> Sequence.of_list and backward = V_ref.vertex in
      One_to_many.{ forward; backward }
    in
    (g_replicated, rel)
end

let%test_module "unshare" =
  (module struct
    module G =
      Graph_ext.Make
        (Int)
        (struct
          include Int

          let default = -1
        end)

    module U =
      Make
        (G)
        (struct
          let kind x = if x mod 2 = 0 then `Args else `State
        end)

    let pp fmt g =
      let pp_vertex = U.V_ref.pp Int.pp in
      U.G_replicated.iter_edges (fun v v' -> Fmt.pf fmt "%a -> %a@." pp_vertex v pp_vertex v') g

    let%expect_test "" =
      let g = G.create () in
      G.add_edge_e g (0, 0, 1);
      G.add_edge_e g (0, 1, 3);
      G.add_edge g 1 4;
      G.add_edge g 3 4;
      let g', _ = U.unshare g in
      Fmt.pr "%a" pp g';
      [%expect {|
0@1 -> 3@2
0@1 -> 1@3
3@2 -> 4@5
1@3 -> 4@4 |}]

    let%expect_test "" =
      let g = G.create () in
      G.add_edge_e g (0, 0, 1);
      G.add_edge_e g (0, 1, 3);
      G.add_edge g 1 2;
      G.add_edge g 1 4;
      G.add_edge g 3 4;
      G.add_edge g 3 6;
      let g', _ = U.unshare g in
      Fmt.pr "%a" pp g';
      [%expect {|
0@1 -> 3@2
0@1 -> 1@3
3@2 -> 6@4
3@2 -> 4@6
1@3 -> 4@5
1@3 -> 2@7 |}]

    let%expect_test "" =
      let g = G.create () in
      G.add_edge_e g (0, 0, 1);
      G.add_edge g 1 2;
      G.add_edge g 1 4;
      let g', _ = U.unshare g in
      Fmt.pr "%a" pp g';
      [%expect {|
           0@1 -> 1@2
           1@2 -> 4@3
           1@2 -> 2@4 |}]

    let%expect_test "" =
      let g = G.create () in
      G.add_edge g 0 1;
      G.add_edge g 1 2;
      G.add_edge g 1 4;
      G.add_edge g 2 3;
      G.add_edge g 4 3;
      let g', _ = U.unshare g in
      Fmt.pr "%a" pp g';
      [%expect
        {|
           0@1 -> 1@2
           1@2 -> 4@3
           1@2 -> 2@4
           2@4 -> 3@5
           4@3 -> 3@5 |}]

    let%expect_test "" =
      let g = G.create () in
      G.add_edge_e g (0, 0, 1);
      G.add_edge_e g (0, 1, 5);
      G.add_edge g 1 2;
      G.add_edge g 1 4;
      G.add_edge g 2 5;
      G.add_edge g 4 5;
      G.add_edge g 5 6;
      G.add_edge g 5 8;
      G.add_edge_e g (8, 0, 9);
      G.add_edge_e g (8, 1, 9);
      let g', _ = U.unshare g in
      Fmt.pr "%a@." pp g';
      [%expect
        {|
0@1 -> 5@2
0@1 -> 1@3
8@11 -> 9@13
8@11 -> 9@14
5@10 -> 8@11
5@10 -> 6@12
5@2 -> 8@4
5@2 -> 6@5
8@4 -> 9@8
8@4 -> 9@9
2@7 -> 5@10
1@3 -> 4@6
1@3 -> 2@7
4@6 -> 5@10 |}]

    let%expect_test "" =
      let g = G.create () in
      G.add_edge g 1 0;
      G.add_edge g 1 2;
      G.add_edge_e g (0, 0, 3);
      G.add_edge_e g (0, 1, 5);
      G.add_edge_e g (2, 0, 5);
      G.add_edge_e g (2, 1, 3);
      G.add_edge g 3 4;
      G.add_edge g 5 6;
      G.add_edge_e g (4, 0, 7);
      G.add_edge_e g (4, 1, 7);
      G.add_edge_e g (6, 0, 9);
      G.add_edge_e g (6, 1, 9);
      let g', _ = U.unshare g in
      Fmt.pr "%a@." pp g';
      [%expect
        {|
           3@5 -> 4@7
           1@1 -> 2@2
           1@1 -> 0@3
           2@2 -> 5@4
           2@2 -> 3@5
           5@4 -> 6@6
           4@7 -> 7@10
           4@7 -> 7@11
           0@3 -> 5@4
           0@3 -> 3@5
           6@6 -> 9@8
           6@6 -> 9@9 |}]
  end)
