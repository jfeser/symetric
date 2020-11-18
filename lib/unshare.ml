module type LABELED_GRAPH = sig
  type vertex

  include
    Graph_ext.GRAPH
      with type vertex := vertex
       and type edge = vertex * int * vertex
end

module One_to_many = struct
  type ('a, 'b) t = { forward : 'a -> 'b Sequence.t; backward : 'b -> 'a }
end

module Make
    (G : LABELED_GRAPH) (K : sig
      val kind : G.V.t -> [ `Args | `State ]
    end) =
struct
  module V_ref = struct
    module T = struct
      type t = { id : int; node : (G.V.t[@ignore]) }
      [@@deriving compare, equal, hash, sexp_of]
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

    let work =
      Hash_queue.create
        {
          hash = G.V.hash;
          compare = G.V.compare;
          sexp_of_t = [%sexp_of: G.V.t];
        }
    in

    let update q key ~f =
      let data' = Hash_queue.lookup q key |> f in
      match Hash_queue.replace q key data' with
      | `Ok -> ()
      | `No_such_key -> Hash_queue.enqueue_back_exn q key data'
    in

    let add_ancestor ancestors port =
      match K.kind port.Port.node.node with
      | `Args -> Set.add ancestors port
      | `State -> ancestors
    in

    let exists_common_ancestor asets =
      Set.union_list (module Port) asets
      |> Set.fold_until
           ~init:(Map.empty (module V_ref))
           ~f:(fun ancestor_ports Port.{ node; port } ->
             match Map.find ancestor_ports node with
             | None -> Continue (Map.set ancestor_ports ~key:node ~data:port)
             | Some port' ->
                 if port = port' then Continue ancestor_ports else Stop true)
           ~finish:(fun _ -> false)
    in

    let rec loop () =
      match Hash_queue.dequeue_front_with_key work with
      | Some (vertex, in_edges) ->
          let any_overlap =
            let _, ancestors = List.unzip in_edges in
            exists_common_ancestor ancestors
          in

          if List.is_empty in_edges then (
            let this_ref = create vertex in

            G_replicated.add_vertex g_replicated this_ref;

            G.succ_e g vertex
            |> List.iter ~f:(fun (_, idx', child) ->
                   let port_ref = Port.{ node = this_ref; port = idx' } in
                   let child_in_edge =
                     (port_ref, add_ancestor (Set.empty (module Port)) port_ref)
                   in
                   update work child ~f:(fun edges ->
                       child_in_edge :: Option.value edges ~default:[])) )
          else if not any_overlap then
            let this_ref = create vertex in
            List.iter in_edges
              ~f:(fun (Port.{ node = parent; port = idx }, ancestors) ->
                G_replicated.add_edge_e g_replicated (parent, idx, this_ref);

                G.succ_e g vertex
                |> List.iter ~f:(fun (_, idx', child) ->
                       let this_port = Port.{ node = this_ref; port = idx' } in
                       let child_in_edge =
                         (this_port, add_ancestor ancestors this_port)
                       in
                       update work child ~f:(fun edges ->
                           child_in_edge :: Option.value edges ~default:[])))
          else
            List.iter in_edges
              ~f:(fun (Port.{ node = parent; port = idx }, ancestors) ->
                let this_ref = create vertex in

                G_replicated.add_edge_e g_replicated (parent, idx, this_ref);

                G.succ_e g vertex
                |> List.iter ~f:(fun (_, idx', child) ->
                       let this_port = Port.{ node = this_ref; port = idx' } in
                       let child_in_edge =
                         (this_port, add_ancestor ancestors this_port)
                       in
                       update work child ~f:(fun edges ->
                           child_in_edge :: Option.value edges ~default:[])));
          loop ()
      | None -> ()
    in

    G.iter_vertex
      (fun v ->
        if G.in_degree g v = 0 then Hash_queue.enqueue_back_exn work v [])
      g;
    loop ();

    let rel =
      let forward v = Map.find_exn !vrefs v |> Sequence.of_list
      and backward = V_ref.vertex in
      One_to_many.{ forward; backward }
    in
    (g_replicated, rel)
end

let%test_module "unshare" =
  ( module struct
    module G = struct
      module Vertex = Int

      module Edge = struct
        type t = int [@@deriving compare]

        let default = -1
      end

      include Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
                (Vertex)
                (Edge)

      module V = struct
        include Int
        include V
      end

      module E = struct
        module T = struct
          type t = V.t * int * V.t [@@deriving compare, hash, sexp_of]
        end

        include T
        include Comparator.Make (T)

        include (
          E :
            sig
              include module type of E
            end
            with type t := t )
      end
    end

    module U =
      Make
        (G)
        (struct
          let kind x = if x mod 2 = 0 then `Args else `State
        end)

    let pp fmt g =
      let pp_vertex = U.V_ref.pp Int.pp in
      U.G_replicated.iter_edges
        (fun v v' -> Fmt.pf fmt "%a -> %a@." pp_vertex v pp_vertex v')
        g

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
      [%expect
        {|
1@8 -> 4@10
1@8 -> 2@12
3@7 -> 6@9
3@7 -> 4@11
0@6 -> 3@7
0@6 -> 1@8 |}]

    let%expect_test "" =
      let g = G.create () in
      G.add_edge_e g (0, 0, 1);
      G.add_edge g 1 2;
      G.add_edge g 1 4;
      let g', _ = U.unshare g in
      Fmt.pr "%a" pp g';
      [%expect
        {|
           0@13 -> 1@14
           1@14 -> 4@15
           1@14 -> 2@16 |}]

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
           2@20 -> 3@21
           4@19 -> 3@21
           1@18 -> 4@19
           1@18 -> 2@20
           0@17 -> 1@18 |}]

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
1@24 -> 4@27
1@24 -> 2@28
4@27 -> 5@31
5@23 -> 8@25
5@23 -> 6@26
0@22 -> 5@23
0@22 -> 1@24
2@28 -> 5@31
8@25 -> 9@29
8@25 -> 9@30
8@32 -> 9@34
8@32 -> 9@35
8@32 -> 9@36
8@32 -> 9@37
5@31 -> 8@32
5@31 -> 6@33 |}]
  end )
